#include "program.hpp"

#include "ast/serializer.hpp"
#include "ast/top_lvl_nodes.hpp"
#include "ast_to_cfg.hpp"
#include "cfg_to_llvm.hpp"
#include "control_flow/type_checker.hpp"
#include "emit_asm.hpp"
#include "jit.hpp"
#include "settings.hpp"
#include "utils/execute.hpp"
#include "utils/string_utils.hpp" // normalized_absolute_path

#include <iostream> // cout, cerr
#include <queue>
#include <set>

#include <llvm/IR/LLVMContext.h>

// Topologically sort the DAG provided by `graph` starting at the `root`
// TODO: test this
static std::vector<std::string>
toposort_dependencies(const std::string & root,
                      const std::map<std::string, std::set<std::string>> & dependency_graph) {
    std::vector<std::string> to_ret;
    std::vector<std::string> eval_stack{root};

    const auto contains = [](const auto & vec, const auto & item) -> bool {
        return std::find(vec.begin(), vec.end(), item) != vec.end();
    };

    while (not eval_stack.empty()) {
        auto & current = eval_stack.back();

        // Prevent duplicate entries in `to_ret`
        if (contains(to_ret, current)) {
            eval_stack.pop_back();
            continue;
        }

        auto current_node_info = dependency_graph.find(current);
        // TODO: should finding that an item is not in the graph be an error?
        if (current_node_info == dependency_graph.end() or current_node_info->second.empty()) {
            // We can return ourselves now
            to_ret.push_back(current);
            eval_stack.pop_back();
            continue;
        }

        // `current_node_info` has a non-zero dependency set
        bool pushed_items = false;
        for (const auto & item : current_node_info->second) {
            if (contains(to_ret, item)) { continue; }

            // The item is not in the return list
            // However, there may be a cycle.
            // We need to check that we are not in the eval_stack already.
            if (contains(eval_stack, item)) { return {}; }

            eval_stack.push_back(item);
            pushed_items = true;
            break;
        }

        if (not pushed_items) {
            to_ret.push_back(current);
            eval_stack.pop_back();
        }
    }

    return to_ret;
}

static std::unique_ptr<ast::top_level_sequence>
read_module(const std::string & filename, const std::filesystem::path & project_root,
            ast::type_context & type_context) {
    auto parser = parser::from_file(filename, project_root, type_context);
    if (parser == nullptr) { return nullptr; }

    auto module_ = parser->parse();
    if (module_ == nullptr) {
        const auto & errors = parser->error_message();
        for (const auto & error : errors) { std::cerr << error << std::endl; }
    }
    return module_;
}

// Breadth first search to load the full module graph
static std::vector<ast::top_level_sequence> load_modules(const std::string & root_filename,
                                                         ast::type_context & type_context) {

    std::vector<ast::top_level_sequence> modules;
    bool failed_parsing = false;

    std::set<std::string> loaded;
    std::queue<std::string> to_load;

    auto proper_root_name = normalized_absolute_path(root_filename);
    const auto project_root = proper_root_name.parent_path();

    to_load.push(std::move(proper_root_name));

    while (not to_load.empty()) {

        auto filename = to_load.front();
        to_load.pop();

        // do not double load files
        if (loaded.find(filename) != loaded.end()) { continue; }

        auto parsed_module = read_module(filename, project_root, type_context);
        if (parsed_module == nullptr) {
            failed_parsing = true;
        } else {
            parsed_module->filename = unquote(filename);

            for (const auto & iter : parsed_module->imports) {
                // certain modules are "pseudo" (only containing intrinsics)
                if (iter.first == "env") { continue; }

                to_load.push(project_root / unquote(iter.first));
            }

            modules.push_back(std::move(*parsed_module));
            parsed_module.reset();
        }
        loaded.insert(std::move(filename));
    }

    if (failed_parsing) { modules.clear(); }

    return modules;
}

std::unique_ptr<program> program::from_root_file(const std::string & root_filename,
                                                 std::shared_ptr<Settings> settings) {
    auto ty_context = std::make_unique<ast::type_context>();
    auto modules = load_modules(root_filename, *ty_context);

    if (modules.empty()) {
        std::cerr << "Could not load modules from root of " << root_filename << std::endl;
        return nullptr;
    }

    // topo sort the modules
    std::map<std::string, std::set<std::string>> dependencies;
    for (auto & mod : modules) {
        auto abs_path = normalized_absolute_path(mod.filename);
        std::set<std::string> imports;
        for (auto & [dependency, _] : mod.imports) {
            // TODO: Make generic for other pseudo-modules
            if (dependency == "env") { continue; }

            imports.emplace(abs_path.parent_path() / dependency);
        }
        dependencies.emplace(abs_path, std::move(imports));
    }
    auto sorted = toposort_dependencies(normalized_absolute_path(root_filename), dependencies);
    if (sorted.empty()) {
        std::cerr << "Found cyclic file dependency.\nCannot process this program" << std::endl;
        return nullptr;
    }

    assert(sorted.size() == modules.size());

    const auto debug_ast = settings->flag_is_set(cmd_flag::debug_ast);
    // swap the given modules into the toposorted order
    auto dest_iter = modules.begin();
    for (auto & file : sorted) {
        auto iter = std::find_if(dest_iter, modules.end(), [&file](const auto & mod) -> bool {
            return mod.filename == normalized_absolute_path(file);
        });
        assert(iter != modules.end());
        assert(dest_iter != modules.end());

        if (debug_ast) { ast::serializer::into_stream(std::cout, file, *iter, true); }

        std::iter_swap(dest_iter, iter);
        dest_iter++;
    }

    return std::unique_ptr<program>{
        new program{std::move(modules), std::move(ty_context), std::move(settings),
                    normalized_absolute_path(root_filename).parent_path()}
    };
}

program::~program() noexcept = default;

program::program(std::vector<ast::top_level_sequence> && modules,
                 std::unique_ptr<ast::type_context> ty_context, std::shared_ptr<Settings> settings,
                 std::string && project_root)
    : project_root{std::move(project_root)}
    , context{std::make_unique<llvm::LLVMContext>()}
    , settings{std::move(settings)}
    , ast_modules(std::move(modules))
    , ty_context{std::move(ty_context)}
    , llvm_lowering{*this->ty_context, context.get()} {}

void program::lower_to_control_flow_graph() {
    ast_to_cfg lowering{*ty_context};

    for (auto & mod : ast_modules) {
        // TODO: Preserve both the absolute and project-relative filepaths
        auto filename = std::filesystem::relative(mod.filename, project_root);
        mod.filename = std::move(filename);
        lowering.visit(mod);
    }

    this->cfg = std::move(lowering).take_cfg();

    if (settings->flag_is_set(cmd_flag::debug_cfg)) { cfg->list_all_nodes(); }
}

bool program::type_check() {
    // TODO: Add the program globals from the env pseudo-module
    control_flow::type_checker checker{*ty_context};

    cfg->for_each_function([&checker](auto * root) {
        assert(root != nullptr);
        // TODO: Do not require the name lookup for visit
        checker.control_flow::visitor::visit(*root);
    });
    return checker.checked_good();
}

void program::generate_ir() {
    global_map<std::string, llvm::GlobalObject *> globals;
    // TODO: Remove `a.out` hard coding
    cfg_to_llvm code_generator{cfg->program_name(), *context, globals, llvm_lowering};
    cfg->for_each_function([this, &code_generator](auto * root) {
        code_generator.control_flow::visitor::visit(*root);

        if (settings->flag_is_set(cmd_flag::debug_ir)) { code_generator.dump(); }
        code_generator.verify_module();
    });
    ir_modules.push_back(std::move(code_generator).take_ir_module());
}

bool program::optimize() {
    auto print_optimized_ir = settings->flag_is_set(cmd_flag::debug_optimized_ir);

    for (auto & mod : ir_modules) {
        if (not optimize_module(*mod, print_optimized_ir)) { return false; }
    }

    return true;
}

std::string program::emit_and_link() {

    const auto debug_print_execs = settings->flag_is_set(cmd_flag::debug_show_execs);

    // TODO: make a better path to stdlib
    // TODO: do not invoke as if start.o already exists
    auto project = std::filesystem::path(project_root);

    auto bootstrap = project / "stdlib" / "start.S";
    if (not exec_command({"as", std::move(bootstrap), "-o", "start.o"}, debug_print_execs)) {
        std::cerr << "Error assembling start.S" << std::endl;
        exit(0);
    }

    std::filesystem::path program_name;
    std::vector<std::string> linker_args{"ld", "-static", "--gc-sections", "start.o"};
    if (debug_print_execs) { linker_args.emplace_back("--print-gc-sections"); }

    for (auto && mod : ir_modules) {
        auto output_name = std::filesystem::path(mod->getSourceFileName()).replace_extension("o");
        linker_args.emplace_back(output_name);
        emit_asm(std::move(mod), std::string{output_name});
        program_name = std::filesystem::current_path() / output_name.stem();
    }

    linker_args.emplace_back("-o");
    linker_args.push_back(std::move(program_name));

    if (not exec_command(std::move(linker_args), debug_print_execs)) {
        std::cerr << "Error linking " << program_name << std::endl;
        exit(0);
    }
    return program_name;
}

uint64_t program::jit() { return run_module(std::move(ir_modules)); }
