#include "ast/nodes.hpp"
#define PARSER_TEST
#include "new_parser.hpp"

#include <catch2/catch.hpp>

TEST_CASE("the parser will not accept empty inputs") {
    std::string buffer;
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

	CHECK(parser->peek_token().first == parser::token_type::eof);

    CHECK(parser->parse() == nullptr);
    CHECK(parser->error_message() == "Found empty file");
}

TEST_CASE("the parser will parse an identifier") {
    std::string buffer = "main";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

	CHECK(parser->peek_token().first == parser::token_type::identifier);
}

TEST_CASE("the parser will parse a function") {
    std::string buffer = "main() -> int = 0";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

	CHECK(parser->peek_token().first == parser::token_type::identifier);

    CHECK(parser->parse() != nullptr);
    CHECK(parser->error_message().empty());
}
