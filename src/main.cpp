#include "context_module.hpp"
#include "emit_asm.hpp"
#include "jit.hpp"
#include "nodes.hpp"
#include "parser.hpp"
#include "settings.hpp"
#include "tokens.hpp"

#include <cassert>
#include <cstdio>
#include <fstream>
#include <iostream>
#include <sstream>

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

int main(const int arg_count, const char * const * const args) {

    const auto command_line = read_settings(arg_count, args);

    // TODO: Move into settings parser
    if (command_line->print_version) {
        std::cout << *args << "\nVersion: 0.0.1" << std::endl;
        return 0;
    }

    const auto & filename = command_line->file_to_read;
    if (filename.empty()) {
        std::cerr << "Input file not specified." << std::endl;
        return 1;
    }

    yyin = fopen(filename.c_str(), "r");

    if (yyin == nullptr) {
        std::cerr << filename << " cannot be opened." << std::endl;
        return -1;
    }

    // If the file could not be parsed, leave the program immediately.
    if (not parse_file()) {
        std::cerr << "Failed to parse " << filename << std::endl;
        return -1;
    }

    assert(module != nullptr);

    auto target_triple = init_llvm_targets();

    context_module context{filename};
    context.module().setTargetTriple(target_triple);

    module->codegen(context);

    context.dump();

    if (command_line->simulate) {
        auto module_result = run_module(std::move(context));
        std::cout << "Module returned " << module_result << std::endl;
    } else {
        emit_asm(std::move(context), std::move(target_triple));
    }
}
