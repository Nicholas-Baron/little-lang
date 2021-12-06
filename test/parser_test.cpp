#include "parser.hpp"
#include "tokens.hpp"
#include <catch2/catch.hpp>

#include <string>

extern std::unique_ptr<ast::top_level_sequence> module;

static int run_parser(std::string && data) {

    auto * buffer = yy_scan_string(data.c_str());

    yy_switch_to_buffer(buffer);

    auto result = yyparse();

    yy_delete_buffer(buffer);

    return result;
}

TEST_CASE("the input cannot be empty") { CHECK(run_parser("") == 1); }

TEST_CASE("a function can be parsed") {
    CHECK(run_parser("main() -> int = 0") == 0);
    CHECK(module != nullptr);
}
