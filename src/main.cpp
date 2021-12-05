#include "ast/top_lvl_nodes.hpp"
#include "emit_asm.hpp"
#include "jit.hpp"
#include "parser.hpp" // yyparse
#include "settings.hpp"
#include "tokens.hpp" // yyin
#include "visitor/codegen.hpp"
#include "visitor/printer.hpp"
#include "visitor/type_checker.hpp"
#include <sys/wait.h> // waitpid

#include <cassert>
#include <cstdio>  // fopen
#include <cstring> // strcpy
#include <iostream>
#include <queue>
#include <set>
#include <unistd.h> // execve
#include <utility>

extern std::unique_ptr<ast::top_level_sequence> module;

static bool parse_file() {

    const auto parse_status = yyparse();

    if (parse_status != 0) {
        std::cerr << "Parsing error: ";
        if (parse_status == 1) {
            std::cerr << "Syntax error found!";
        } else if (parse_status == 2) {
            std::cerr << "Exhausted memory!";
        } else {
            std::cerr << "Unknown error";
        }

        std::cerr << std::endl;
    }

    /* According to bison, a yyparse() result of 0 is successful.
     * (ie: read all input) */
    return parse_status == 0;
}

static auto read_module(const std::string & filename) -> decltype(module) {
    if (filename.empty()) {
        std::cerr << "Input file not specified." << std::endl;
        return nullptr;
    }

    yyin = fopen(filename.c_str(), "r");

    if (yyin == nullptr) {
        std::cerr << filename << " cannot be opened." << std::endl;
        return nullptr;
    }

    // If the file could not be parsed, leave the program immediately.
    if (not parse_file()) {
        std::cerr << "Failed to parse " << filename << std::endl;
        return nullptr;
    }

    return std::move(module);
}

static std::vector<ast::top_level_sequence> load_modules(std::string input, bool debug_ast) {

    std::vector<ast::top_level_sequence> modules;

    std::set<std::string> loaded;
    std::queue<std::string> to_load;
    to_load.push(std::move(input));

    while (not to_load.empty()) {

        auto filename = to_load.front();
        to_load.pop();

        if (loaded.find(filename) != loaded.end()) { continue; }

        auto parsed_module = read_module(filename);
        if (parsed_module == nullptr) {
            std::cout << "Failed to parse " << filename << std::endl;
            assert(false);
        }

        parsed_module->filename = filename;

        if (debug_ast) {
            visitor::printer printer_visitor{filename};
            printer_visitor.visit(*parsed_module);
        }

        modules.push_back(std::move(*parsed_module));
        parsed_module.reset();
        loaded.insert(std::move(filename));
    }

    return modules;
}

static bool exec_command(std::vector<std::string> && cmd, bool debug) {

    if (debug) {
        std::cout << "[CMD] ";
        for (const auto & arg : cmd) { std::cout << arg << ' '; }
        std::cout << std::endl;
    }

    if (auto pid = fork(); pid == 0) {
        // in child

        std::vector<char *> args;
        args.reserve(cmd.size());
        for (auto & arg : cmd) { args.emplace_back(strcpy(new char[arg.size() + 1], arg.c_str())); }
        args.push_back(nullptr);

        if (execvp(args[0], args.data()) == -1) {
            perror("execvp");
            exit(-1);
        } else {
            exit(0);
        }
    } else if (pid == -1) {
        perror("fork");
        return false;
    } else {

        int wait_status;
        if (waitpid(pid, &wait_status, 0) != pid) {
            perror("waitpid");
            return false;
        }

        if (not WIFEXITED(wait_status)) { return false; }

        return WEXITSTATUS(wait_status) == 0;
    }
}

int main(const int arg_count, const char * const * const args) {

    const auto command_line = read_settings(arg_count, args);

    const auto & filename = command_line->file_to_read;

    auto modules = load_modules(filename, command_line->flag_is_set(cmd_flag::debug_ast));
    assert(modules.size() == 1);

    for (auto & parsed_module : modules) {

        {
            visitor::type_checker type_checker;
            type_checker.visit(parsed_module);
            if (not type_checker.checked_good()) {
                std::cout << "Failed to type check" << std::endl;
                return 1;
            }
        }

        visitor::codegen codegen_visitor{filename};
        codegen_visitor.visit(parsed_module);

        codegen_visitor.verify_module();

        if (command_line->flag_is_set(cmd_flag::debug_ir)) { codegen_visitor.dump(); }

        if (command_line->flag_is_set(cmd_flag::simulate) and modules.size() == 1) {
            auto parsed_module_result = run_module(std::move(codegen_visitor).take_ir_module());
            std::cout << "parsed_module returned " << parsed_module_result << std::endl;
        } else {
            auto output_name = make_output_name(filename);
            emit_asm(std::move(codegen_visitor).take_ir_module(), std::string{output_name});

            auto program_name = output_name.substr(0, output_name.find_last_of('.'));

            // TODO: Get away from C's standard library
            exec_command({"gcc", "-static", "-o", std::move(program_name), std::move(output_name)},
                         command_line->flag_is_set(cmd_flag::debug_show_execs));
        }
    }
}
