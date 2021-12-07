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

    auto tok = parser->next_token();
    CHECK(tok.first == parser::token_type::identifier);
    CHECK(tok.second == "main");
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse a plain string") {
    std::string buffer = "\"raw\"";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto tok = parser->next_token();
    CHECK(tok.first == parser::token_type::string);
    CHECK(tok.second == "\"raw\"");
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse an integer") {
    std::string buffer = "1234";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto tok = parser->next_token();
    CHECK(tok.first == parser::token_type::integer);
    CHECK(tok.second == "1234");
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse a hexadecimal integer") {
    std::string buffer = "0x123456789aBcDeF";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto tok = parser->next_token();
    CHECK(tok.first == parser::token_type::integer);
    CHECK(tok.second == buffer);
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

TEST_CASE("the parser will parse a comma") {
    std::string buffer = ",";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::comma);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse a semicolon") {
    std::string buffer = ";";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::semi);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse 'from', 'import', and 'export'") {
    std::string buffer = "from import export";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::from);
    CHECK(parser->next_token().first == parser::token_type::import_);
    CHECK(parser->next_token().first == parser::token_type::export_);
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

    auto func = parser->parse_function();
    CHECK(func != nullptr);
    CHECK(parser->error_message().empty());

    CHECK(func->head.name() == "main");
    CHECK(func->head.param_count() == 0);
    CHECK(func->head.ret_type().empty());
    CHECK(func->body != nullptr);
}

TEST_CASE("the parser will parse a function with return type") {
    std::string buffer = "main() -> int {}";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto func = parser->parse_function();
    CHECK(func != nullptr);
    CHECK(parser->error_message().empty());

    CHECK(func->head.name() == "main");
    CHECK(func->head.param_count() == 0);
    CHECK(func->head.ret_type() == "int");
    CHECK(func->body != nullptr);
}

TEST_CASE("the parser will parse a function with parameters") {
    std::string buffer = "foo(int x, y : bool) -> int {}";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto func = parser->parse_function();
    CHECK(func != nullptr);
    CHECK(parser->error_message().empty());

    CHECK(func->head.name() == "foo");
    CHECK(func->head.param_count() == 2);
    CHECK(func->head.arg(0).name() == "x");
    CHECK(func->head.arg(0).type() == "int");
    CHECK(func->head.arg(1).name() == "y");
    CHECK(func->head.arg(1).type() == "bool");
    CHECK(func->head.ret_type() == "int");
    CHECK(func->body != nullptr);
}

TEST_CASE("the parser will parse a module with imports") {
    std::string buffer = "from \"test.lil\" import foo, bar; main() {}";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);
    CHECK(parser->peek_token().first == parser::token_type::from);

    auto mod = parser->parse();
    CHECK(mod != nullptr);
    CHECK(parser->error_message().empty());
    std::cout << parser->error_message() << std::endl;

    CHECK(mod->imports.size() == 1);
    {
        auto iter = mod->imports.find("test.lil");
        CHECK_FALSE(iter == mod->imports.end());
        CHECK(iter->second.size() == 2);
    }

    CHECK(mod->items.size() == 1);
    CHECK_FALSE(mod->items.at(0) == nullptr);
}
