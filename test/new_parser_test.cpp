#include "ast/nodes.hpp"

#include <iostream>
#define PARSER_TEST
#include "new_parser.hpp"
#include <catch2/catch.hpp>

// TODO: This split in the tests should probably be reflected in a lexer-parser split

// token level
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

    CHECK(parser->next_token().first == parser::token_type::identifier);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse a colon and the word 'is' as the same token") {
    std::string buffer = "is:";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::colon);
    CHECK(parser->next_token().first == parser::token_type::colon);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse parentheses") {
    std::string buffer = "()";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::lparen);
    CHECK(parser->next_token().first == parser::token_type::rparen);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse braces") {
    std::string buffer = "{}";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::lbrace);
    CHECK(parser->next_token().first == parser::token_type::rbrace);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse a 'skinny' arrow") {
    std::string buffer = "->";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::arrow);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

// ast level
TEST_CASE("the parser will parse braces as a compound statement") {
    std::string buffer = "{}";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_compound_statement();
    CHECK(stmt != nullptr);
}

TEST_CASE("the parser will parse a unit function") {
    std::string buffer = "main() {}";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->peek_token().first == parser::token_type::identifier);

    CHECK(parser->parse() != nullptr);
    CHECK(parser->error_message().empty());
    std::cout << parser->error_message() << std::endl;
}

TEST_CASE("the parser will parse a function with return type") {
    std::string buffer = "main() -> int {}";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->peek_token().first == parser::token_type::identifier);

    CHECK(parser->parse() != nullptr);
    CHECK(parser->error_message().empty());
    std::cout << parser->error_message() << std::endl;
}
