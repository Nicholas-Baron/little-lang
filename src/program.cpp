#include "program.hpp"

#include "ast/node_utils.hpp"
#include "ast/top_lvl_nodes.hpp"
#include "emit_asm.hpp"
#include "global_map.hpp"
#include "jit.hpp"
#include "utils/execute.hpp"
#include "utils/string_utils.hpp"
#include "visitor/codegen.hpp"
#include "visitor/printer.hpp"
#include "visitor/type_checker.hpp"

#include <cstring> // strlcpy
#include <filesystem>
#include <iostream>
#include <set>

#include <llvm/IR/LLVMContext.h>

// TODO: test this
static std::vector<std::string>
toposort(const std::string & root, const std::map<std::string, std::set<std::string>> & graph) {
    std::vector<std::string> to_ret;
    std::vector<std::string> eval_stack{root};

    const auto contains = [](const auto & vec, const auto & item) -> bool {
        return std::find(vec.begin(), vec.end(), item) != vec.end();
    };

    while (not eval_stack.empty()) {
        auto & current = eval_stack.back();
        // fmt::print("\nstack size: {}\nstack: {}\nevaluating {}\n",
        //         eval_stack.size(), eval_stack, current);
        // if (not to_ret.empty()) fmt::print("result: {}\n", to_ret);

        if (contains(to_ret, current)) {
            eval_stack.pop_back();
            continue;
        }

        auto iter = graph.find(current);
        // TODO: should finding that an item is not in the graph be an error?
        if (iter == graph.end() or iter->second.empty()) {
            // we can return ourselves now
            to_ret.push_back(current);
            // fmt::print("pushing {}\n", current);
            eval_stack.pop_back();
            continue;
        }

        // current has a non-zero dependency set
        bool pushed_items = false;
        for (const auto & item : iter->second) {
            if (contains(to_ret, item)) { continue; }

            // fmt::print("could not find {}\n", item);
            //
            // the item is not in the return list
            // however, there may be a cycle.
            // we need to check that we are not in the eval_stack already
            if (contains(eval_stack, item)) {
                // fmt::print("Cycle found: {}\n", eval_stack);
                return {};
            }

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

std::optional<program> program::from_modules(const std::string & root_file,
                                             std::vector<ast::top_level_sequence> && modules,
                                             std::shared_ptr<Settings> settings) {
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
    auto sorted = toposort(normalized_absolute_path(root_file), dependencies);
    if (sorted.empty()) {
        std::cerr << "Found cyclic file dependency.\nCannot process this program" << std::endl;
        return std::nullopt;
    }

    assert(sorted.size() == modules.size());

    // swap the given modules into the toposorted order
    auto dest_iter = modules.begin();
    for (auto & file : sorted) {
        auto iter = std::find_if(dest_iter, modules.end(), [&file](const auto & mod) -> bool {
            return mod.filename == normalized_absolute_path(file);
        });
        assert(iter != modules.end());
        assert(dest_iter != modules.end());
        std::iter_swap(dest_iter, iter);
        dest_iter++;
    }

    return program{std::move(modules), std::move(settings),
                   normalized_absolute_path(root_file).parent_path()};
}

program::program(std::vector<ast::top_level_sequence> && modules,
                 std::shared_ptr<Settings> settings, std::string && project_root)
    : project_root{std::move(project_root)}
    , context{std::make_unique<llvm::LLVMContext>()}
    , settings{std::move(settings)}
    , ast_modules(std::move(modules))
    , typ_context(context.get()) {}

bool program::type_check() {
    global_map<std::string, ast::type_ptr> program_globals;
    program_globals.add("env", "arg_count",
                        std::make_shared<ast::function_type>(ast::prim_type::int32));
    program_globals.add("env", "arg_at",
                        std::make_shared<ast::function_type>(ast::prim_type::str,
                                                             std::vector{ast::prim_type::int32}));

    for (auto & mod : ast_modules) {
        // Note: currently, the ast imports are not updated with absolute paths,
        // but the ast filenames are absolute paths.
        auto filename = std::filesystem::relative(mod.filename, project_root);
        visitor::type_checker type_checker{std::move(filename), *context, program_globals};
        type_checker.visit(mod);
        if (not type_checker.checked_good()) {
            std::cout << "Failed to type check" << std::endl;
            return false;
        }
    }
    return true;
}

void program::generate_ir() {
    global_map<std::string, llvm::GlobalObject *> globals;
    for (auto & mod : ast_modules) {
        auto filename = std::filesystem::relative(mod.filename, project_root);
        visitor::codegen codegen{filename, *context, globals, typ_context};
        codegen.visit(mod);
        if (settings->flag_is_set(cmd_flag::debug_ir)) { codegen.dump(); }
        codegen.verify_module();
        ir_modules.push_back(std::move(codegen).take_ir_module());
    }
}

std::string program::emit_and_link() {

    const auto debug_print_execs = settings->flag_is_set(cmd_flag::debug_show_execs);

    // TODO: make a better path to stdlib
    // TODO: do not invoke as if start.o already exists
    auto main_file = std::filesystem::path(ast_modules.back().filename);

    auto bootstrap = main_file.parent_path() / "stdlib/start.S";
    if (not exec_command({"as", std::move(bootstrap), "-o", "start.o"}, debug_print_execs)) {
        std::cerr << "Error assembling start.S" << std::endl;
        exit(0);
    }

    auto program_name = std::filesystem::current_path() / main_file.stem();
    std::vector<std::string> linker_args{"ld", "-static",    "--gc-sections",
                                         "-o", program_name, "start.o"};
    if (debug_print_execs) { linker_args.emplace_back("--print-gc-sections"); }

    for (auto && mod : ir_modules) {
        auto output_name = std::filesystem::path(mod->getSourceFileName()).replace_extension("o");
        linker_args.emplace_back(output_name);
        emit_asm(std::move(mod), std::string{output_name},
                 settings->flag_is_set(cmd_flag::debug_optimized_ir));
    }

    if (not exec_command(std::move(linker_args), debug_print_execs)) {
        std::cerr << "Error linking " << program_name << std::endl;
        exit(0);
    }
    return program_name;
}

uint64_t program::jit() { return run_module(std::move(ir_modules)); }
