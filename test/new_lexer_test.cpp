#include "ast/lexer.hpp"

#include <catch2/catch.hpp>

TEST_CASE("the lexer will not accept empty inputs") {
    std::string buffer;
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->peek_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will report locations for tokens") {
    std::string buffer = "foo\n  bar";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token().location == Location{1, 0});
    CHECK(lexer->next_token().location == Location{2, 2});
    CHECK(lexer->next_token().location == Location{2, 5});
}

TEST_CASE("the lexer can look ahead for whole text fragments") {
    std::string buffer = "foo bar baz";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_chars("foo"));
    CHECK(lexer->next_chars("oo", 1));
    CHECK(lexer->next_chars("bar", 4));
    CHECK(lexer->next_chars("baz", 8));
}

TEST_CASE("the lexer will ignore comments") {
    std::string buffer = R"(// this is a comment
                         # this is another comment
                         comment I am from the 1960s
                         Comment I am also from the 1960s
                         foo bar baz)";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == "foo");
    CHECK(lexer->next_token() == "bar");
    CHECK(lexer->next_token() == "baz");
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse an identifier") {
    std::string buffer = "main";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    auto tok = lexer->next_token();
    CHECK(tok == lexer::token_type::identifier);
    CHECK(tok == "main");
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse primitive types") {
    std::string buffer = "unit string char bool float";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::prim_type);
    CHECK(lexer->next_token() == lexer::token_type::prim_type);
    CHECK(lexer->next_token() == lexer::token_type::prim_type);
    CHECK(lexer->next_token() == lexer::token_type::prim_type);
    CHECK(lexer->next_token() == lexer::token_type::prim_type);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse int types") {
    std::string buffer = "int8 int16 int32 int64";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::prim_type);
    CHECK(lexer->next_token() == lexer::token_type::prim_type);
    CHECK(lexer->next_token() == lexer::token_type::prim_type);
    CHECK(lexer->next_token() == lexer::token_type::prim_type);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse an identifier containing underscores") {
    std::string buffer = "my_value";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    auto tok = lexer->next_token();
    CHECK(tok == lexer::token_type::identifier);
    CHECK(tok == "my_value");
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse an identifier starting with underscores") {
    std::string buffer = "_value";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    auto tok = lexer->next_token();
    CHECK(tok == lexer::token_type::identifier);
    CHECK(tok == "_value");
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse a plain string") {
    std::string buffer = "\"raw\"";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    auto tok = lexer->next_token();
    CHECK(tok == lexer::token_type::string);
    CHECK(tok == "\"raw\"");
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse a string with escaped character") {
    std::string buffer = R"("raw\n")";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    auto tok = lexer->next_token();
    CHECK(tok == lexer::token_type::string);
    CHECK(tok == "\"raw\n\"");
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse a character literal") {
    std::string buffer = "\'w\'";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    auto tok = lexer->next_token();
    CHECK(tok == lexer::token_type::character);
    CHECK(tok == "\'w\'");
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse boolean literals") {
    std::string buffer = " true\n false ";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    auto tok = lexer->next_token();
    CHECK(tok == lexer::token_type::boolean);
    CHECK(tok == "true");

    tok = lexer->next_token();
    CHECK(tok == lexer::token_type::boolean);
    CHECK(tok == "false");

    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse an integer") {
    std::string buffer = "1234";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    auto tok = lexer->next_token();
    CHECK(tok == lexer::token_type::integer);
    CHECK(tok == "1234");
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse a hexadecimal integer") {
    std::string buffer = "0x123456789aBcDeF";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    auto tok = lexer->next_token();
    CHECK(tok == lexer::token_type::integer);
    CHECK(tok == buffer);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse a colon and the word 'is' as the same token") {
    std::string buffer = "is:";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::colon);
    CHECK(lexer->next_token() == lexer::token_type::colon);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse parentheses") {
    std::string buffer = "()";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::lparen);
    CHECK(lexer->next_token() == lexer::token_type::rparen);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse braces") {
    std::string buffer = "{}";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::lbrace);
    CHECK(lexer->next_token() == lexer::token_type::rbrace);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse a 'skinny' arrow") {
    std::string buffer = "->";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::arrow);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse a comma") {
    std::string buffer = ",";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::comma);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse a semicolon") {
    std::string buffer = ";";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::semi);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse 'from', 'import', and 'export'") {
    std::string buffer = "from import export";

    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::from);
    CHECK(lexer->next_token() == lexer::token_type::import_);
    CHECK(lexer->next_token() == lexer::token_type::export_);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse 'if', 'then', and 'else'") {
    std::string buffer = "if else then";

    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::if_);
    CHECK(lexer->next_token() == lexer::token_type::else_);
    CHECK(lexer->next_token() == lexer::token_type::then);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse 'return' and 'ret' the same") {
    std::string buffer = "return ret";

    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::return_);
    CHECK(lexer->next_token() == lexer::token_type::return_);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse '<' and '>'") {
    std::string buffer = "< >";

    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::lt);
    CHECK(lexer->next_token() == lexer::token_type::gt);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse '<=' and '>=' as 1 token each") {
    std::string buffer = "<= >=";

    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::le);
    CHECK(lexer->next_token() == lexer::token_type::ge);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse 'let' and 'const'") {
    std::string buffer = "let const";

    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::let);
    CHECK(lexer->next_token() == lexer::token_type::const_);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse 'and' as '&&'") {
    std::string buffer = "and &&";

    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::double_and);
    CHECK(lexer->next_token() == lexer::token_type::double_and);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse 'or' as '||'") {
    std::string buffer = "or ||";

    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::double_or);
    CHECK(lexer->next_token() == lexer::token_type::double_or);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse 'equals' as '=='") {
    std::string buffer = "equals ==";

    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::eq);
    CHECK(lexer->next_token() == lexer::token_type::eq);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse basic mathematical symbols") {
    std::string buffer = "= + - / * %";

    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());

    CHECK(lexer != nullptr);

    CHECK(lexer->next_token() == lexer::token_type::equal);
    CHECK(lexer->next_token() == lexer::token_type::plus);
    CHECK(lexer->next_token() == lexer::token_type::minus);
    CHECK(lexer->next_token() == lexer::token_type::slash);
    CHECK(lexer->next_token() == lexer::token_type::asterik);
    CHECK(lexer->next_token() == lexer::token_type::percent);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse pointer-related tokens") {
    std::string buffer = " & ? null";

    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());
    CHECK(lexer != nullptr);
    CHECK(lexer->next_token() == lexer::token_type::amp);
    CHECK(lexer->next_token() == lexer::token_type::question);
    CHECK(lexer->next_token() == lexer::token_type::null);
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse the '.' as a distinct token") {
    // TODO: Disambiguate the following case
    // let t = ((a, b), y)
    // t.0
    // t.1
    // t.0.0 : t. (0.0) or (t.0).0

    std::string buffer = "x.y";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());
    CHECK(lexer != nullptr);
    CHECK(lexer->next_token() == "x");
    CHECK(lexer->next_token() == lexer::token_type::dot);
    CHECK(lexer->next_token() == "y");
    CHECK(lexer->next_token() == lexer::token_type::eof);
}

TEST_CASE("the lexer will parse the word 'as'") {

    std::string buffer = "x as y";
    auto lexer = lexer::from_buffer(buffer.c_str(), buffer.size());
    CHECK(lexer != nullptr);
    CHECK(lexer->next_token() == "x");
    CHECK(lexer->next_token() == lexer::token_type::as);
    CHECK(lexer->next_token() == "y");
    CHECK(lexer->next_token() == lexer::token_type::eof);
}
