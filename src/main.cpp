#include "ast/top_lvl_nodes.hpp"
#include "emit_asm.hpp"
#include "jit.hpp"
#include "new_parser.hpp"
#include "program.hpp"
#include "settings.hpp"
#include "utils/execute.hpp"
#include "utils/string_utils.hpp"
#include "visitor/codegen.hpp"
#include "visitor/printer.hpp"
#include "visitor/type_checker.hpp"

#include <cassert>
#include <filesystem>
#include <iostream>
#include <iterator>
#include <queue>
#include <set>
#include <utility>

static std::unique_ptr<ast::top_level_sequence> read_module(const std::string & filename) {
    auto p = parser::from_file(filename);
    if (p == nullptr) { return nullptr; }

    auto module_ = p->parse();
    if (module_ == nullptr) { std::cerr << p->error_message() << std::endl; }
    return module_;
}

static std::vector<ast::top_level_sequence> load_modules(const std::string & input,
                                                         bool debug_ast) {

    std::vector<ast::top_level_sequence> modules;

    std::set<std::string> loaded;
    std::queue<std::string> to_load;

    auto proper_input = normalized_absolute_path(input);
    const auto project_root = proper_input.parent_path();

    to_load.push(std::move(proper_input));

    while (not to_load.empty()) {

        auto filename = to_load.front();
        to_load.pop();

        // do not double load files
        if (loaded.find(filename) != loaded.end()) { continue; }

        auto parsed_module = read_module(filename);
        if (parsed_module == nullptr) {
            std::cout << "Failed to parse " << filename << std::endl;
            assert(false);
        }

        parsed_module->filename = unquote(filename);

        if (debug_ast) {
            visitor::printer printer_visitor{filename};
            printer_visitor.visit(*parsed_module);
        }

        for (const auto & iter : parsed_module->imports) {
            // certain modules are "pseudo" (only containing intrinsics)
            if (iter.first == "env") { continue; }

            to_load.push(project_root / unquote(iter.first));
        }

        modules.push_back(std::move(*parsed_module));
        parsed_module.reset();
        loaded.insert(std::move(filename));
    }

    return modules;
}

int main(const int arg_count, const char * const * const args) {

    const auto command_line = read_settings(arg_count, args);

    const auto & filename = command_line->file_to_read;

    auto modules = load_modules(filename, command_line->flag_is_set(cmd_flag::debug_ast));

    assert(not modules.empty());
    auto opt_program = program::from_modules(filename, std::move(modules), command_line);
    if (not opt_program.has_value()) { return 1; }

    auto program = std::move(opt_program).value();
    if (not program.type_check()) { return 2; }
    program.generate_ir();

    if (command_line->flag_is_set(cmd_flag::simulate)) {
        std::cout << "Return value: " << program.jit() << std::endl;
        return 0;
    }

    auto program_name = program.emit_and_link();
    if (command_line->flag_is_set(cmd_flag::run_result)) {
        auto args = std::vector{program_name};
        std::copy(command_line->extra_args.begin(), command_line->extra_args.end(),
                  std::back_inserter(args));
        if (not exec_command(std::move(args),
                             command_line->flag_is_set(cmd_flag::debug_show_execs))) {
            std::cerr << "Could not run " << program_name << std::endl;
        }
    }
}
