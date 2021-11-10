#include "context_module.hpp"
#include "emit_asm.hpp"
#include "jit.hpp"
#include "parser.hpp" // yyparse
#include "settings.hpp"
#include "tokens.hpp" // yyin
#include <sys/wait.h> // waitpid

#include <cassert>
#include <cstdio>  // fopen
#include <cstring> // strcpy
#include <iostream>
#include <unistd.h> // execve

extern std::unique_ptr<Top_Level_Seq> module;

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

static std::unique_ptr<Top_Level_Seq> read_module(const std::string & filename) {
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

    // TODO: Move into settings parser
    if (command_line->print_version) {
        std::cout << *args << "\nVersion: 0.0.1" << std::endl;
        return 0;
    }

    const auto & filename = command_line->file_to_read;

    auto target_triple = init_llvm_targets();

    context_module context{filename};
    context.module().setTargetTriple(target_triple);

    // TODO: Add include/import system
    auto parsed_module = read_module(filename);
    if (parsed_module == nullptr) { return -1; }

    parsed_module->codegen(context);

    context.verify_module();

    if (command_line->debug) { context.dump(); }

    if (command_line->simulate) {
        auto parsed_module_result = run_module(std::move(context));
        std::cout << "parsed_module returned " << parsed_module_result << std::endl;
    } else {
        auto output_name = make_output_name(filename);
        emit_asm(std::move(context), std::move(target_triple), std::string{output_name});

        auto program_name = output_name.substr(0, output_name.find_last_of('.'));

        // TODO: Get away from C's standard library
        exec_command({"gcc", "-static", "-o", std::move(program_name), std::move(output_name)},
                     command_line->debug);
    }
}
