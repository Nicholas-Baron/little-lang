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

TEST_CASE("the parser will parse an identifier with underscores") {
    std::string buffer = "my_value";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto tok = parser->next_token();
    CHECK(tok.first == parser::token_type::identifier);
    CHECK(tok.second == "my_value");
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

TEST_CASE("the parser will parse a character literal") {
    std::string buffer = "\'w\'";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto tok = parser->next_token();
    CHECK(tok.first == parser::token_type::character);
    CHECK(tok.second == "\'w\'");
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

TEST_CASE("the parser will parse 'if' and 'else'") {
    std::string buffer = "if else";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::if_);
    CHECK(parser->next_token().first == parser::token_type::else_);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse 'return' and 'ret' the same") {
    std::string buffer = "return ret";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::return_);
    CHECK(parser->next_token().first == parser::token_type::return_);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse '<' and '>'") {
    std::string buffer = "< >";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::lt);
    CHECK(parser->next_token().first == parser::token_type::gt);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse '<=' and '>=' as 1 token each") {
    std::string buffer = "<= >=";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::le);
    CHECK(parser->next_token().first == parser::token_type::ge);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse 'let' and 'const'") {
    std::string buffer = "let const";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::let);
    CHECK(parser->next_token().first == parser::token_type::const_);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse 'and' as '&&'") {
    std::string buffer = "and &&";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::double_and);
    CHECK(parser->next_token().first == parser::token_type::double_and);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse 'or' as '||'") {
    std::string buffer = "or ||";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::double_or);
    CHECK(parser->next_token().first == parser::token_type::double_or);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse 'equals' as '=='") {
    std::string buffer = "equals ==";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::eq);
    CHECK(parser->next_token().first == parser::token_type::eq);
    CHECK(parser->next_token().first == parser::token_type::eof);
}

TEST_CASE("the parser will parse basic mathematical symbols") {
    std::string buffer = "= + - / * %";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().first == parser::token_type::equal);
    CHECK(parser->next_token().first == parser::token_type::plus);
    CHECK(parser->next_token().first == parser::token_type::minus);
    CHECK(parser->next_token().first == parser::token_type::slash);
    CHECK(parser->next_token().first == parser::token_type::asterik);
    CHECK(parser->next_token().first == parser::token_type::percent);
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

TEST_CASE("the parser will parse let statement") {
    std::string buffer = "let x = \"hello\";";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    CHECK(parser->peek_token().first == parser::token_type::eof);

    auto * let = dynamic_cast<ast::let_stmt *>(stmt.get());
    CHECK(let != nullptr);
    CHECK(let->name_and_type.name() == "x");
    CHECK(let->name_and_type.type() == "auto");
    CHECK(let->value != nullptr);

    auto * value = dynamic_cast<ast::user_val *>(let->value.get());
    CHECK(value != nullptr);
    CHECK(value->type == ast::user_val::value_type::string);
    CHECK(value->val == "\"hello\"");
}

TEST_CASE("the parser will parse unary minus") {
    std::string buffer = "-3";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto expr = parser->parse_expression();
    CHECK(expr != nullptr);
    CHECK(parser->peek_token().first == parser::token_type::eof);

    auto * unary_expr = dynamic_cast<ast::unary_expr *>(expr.get());
    CHECK(unary_expr != nullptr);
    CHECK(unary_expr->expr != nullptr);
    CHECK(unary_expr->op == ast::unary_expr::operand::negate);

    auto * val = dynamic_cast<ast::user_val *>(unary_expr->expr.get());
    CHECK(val != nullptr);
    CHECK(val->type == ast::user_val::value_type::integer);
    CHECK(val->val == "3");
}

TEST_CASE("the parser will parse const declaration") {
    std::string buffer = "const x : int = 5 * -3;";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto decl = parser->parse_top_level();
    CHECK(decl != nullptr);
    CHECK(parser->peek_token().first == parser::token_type::eof);

    auto * const_decl = dynamic_cast<ast::const_decl *>(decl.get());
    CHECK(const_decl != nullptr);
    CHECK(const_decl->name_and_type.name() == "x");
    CHECK(const_decl->name_and_type.type() == "int");
    CHECK(const_decl->expr != nullptr);

    auto * value = dynamic_cast<ast::binary_expr *>(const_decl->expr.get());
    CHECK(value != nullptr);
    CHECK(value->op == ast::binary_expr::operand::mult);
    CHECK(value->lhs != nullptr);
    CHECK(value->rhs != nullptr);
}

TEST_CASE("the parser will parse binary expressions") {
    std::string buffer = " 5 + 10 ";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_expression();
    CHECK(stmt != nullptr);
}

TEST_CASE("the parser will parse if statements") {
    std::string buffer = "if x {}";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    auto * if_stmt = dynamic_cast<ast::if_stmt *>(stmt.get());
    CHECK(if_stmt != nullptr);
    CHECK(if_stmt->else_branch == nullptr);
    CHECK(if_stmt->true_branch != nullptr);

    auto * cond = dynamic_cast<ast::user_val *>(if_stmt->condition.get());
    CHECK(cond != nullptr);
    CHECK(cond->type == ast::user_val::value_type::identifier);
    CHECK(cond->val == "x");
}

TEST_CASE("the parser will parse if-else statements") {
    std::string buffer = "if x {} else {}";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    auto * if_stmt = dynamic_cast<ast::if_stmt *>(stmt.get());
    CHECK(if_stmt != nullptr);
    CHECK(if_stmt->else_branch != nullptr);
    CHECK(if_stmt->true_branch != nullptr);

    auto * cond = dynamic_cast<ast::user_val *>(if_stmt->condition.get());
    CHECK(cond != nullptr);
    CHECK(cond->type == ast::user_val::value_type::identifier);
    CHECK(cond->val == "x");
}

TEST_CASE("the parser will parse return statements") {
    std::string buffer = "return;";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    auto * ret_stmt = dynamic_cast<ast::return_stmt *>(stmt.get());
    CHECK(ret_stmt != nullptr);
    CHECK(ret_stmt->value == nullptr);
}

TEST_CASE("the parser will parse return statements with values") {
    std::string buffer = "return 5 * 10;";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    auto * ret_stmt = dynamic_cast<ast::return_stmt *>(stmt.get());
    CHECK(ret_stmt != nullptr);
    CHECK(ret_stmt->value != nullptr);

    auto * value = dynamic_cast<ast::binary_expr *>(ret_stmt->value.get());
    CHECK(value != nullptr);
    CHECK(value->op == ast::binary_expr::operand::mult);
    CHECK(value->lhs != nullptr);
    CHECK(value->rhs != nullptr);
}

TEST_CASE("the parser will parse function calls as expressions") {
    std::string buffer = "return foo(5, 'x');";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    auto * ret_stmt = dynamic_cast<ast::return_stmt *>(stmt.get());
    CHECK(ret_stmt != nullptr);
    CHECK(ret_stmt->value != nullptr);

    auto * value = dynamic_cast<ast::func_call_expr *>(ret_stmt->value.get());
    CHECK(value != nullptr);
    CHECK(value->data.name() == "foo");
    CHECK(value->data.args_count() == 2);
    CHECK(value->data.arg(0) != nullptr);
    CHECK(value->data.arg(1) != nullptr);
}

TEST_CASE("the parser will parse function calls as statements") {
    std::string buffer = "{ foo(5, 'x'); }";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    auto * stmt_seq = dynamic_cast<ast::stmt_sequence *>(stmt.get());
    CHECK(stmt_seq != nullptr);
    CHECK(stmt_seq->stmts.size() == 1);

    auto * func_call = dynamic_cast<ast::func_call_stmt *>(stmt_seq->stmts[0].get());
    CHECK(func_call != nullptr);
    CHECK(func_call->data.name() == "foo");
    CHECK(func_call->data.args_count() == 2);
    CHECK(func_call->data.arg(0) != nullptr);
    CHECK(func_call->data.arg(1) != nullptr);
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
    CHECK(func->head.ret_type() == "unit");
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

TEST_CASE("the parser will parse a function with an expression body") {
    std::string buffer = "foo(int x, y : int) -> int = (x + y) / 2";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto func = parser->parse_function();
    CHECK(func != nullptr);
    CHECK(parser->error_message().empty());

    CHECK(func->head.name() == "foo");
    CHECK(func->head.param_count() == 2);
    CHECK(func->head.ret_type() == "int");
    CHECK(func->body != nullptr);

    auto * ret_stmt = dynamic_cast<ast::return_stmt *>(func->body.get());
    CHECK(ret_stmt != nullptr);
    CHECK(ret_stmt->value != nullptr);

    auto * value = dynamic_cast<ast::binary_expr *>(ret_stmt->value.get());
    CHECK(value != nullptr);
    CHECK(value->lhs != nullptr);
    CHECK(value->rhs != nullptr);
    CHECK(value->op == ast::binary_expr::operand::div);
}

TEST_CASE("the parser will parse a factorial function") {
    std::string buffer = R"(
factorial(int input) -> int {
	if(input <= 2){ return input; }
	return input * factorial(input - 1);
})";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto func = parser->parse_function();
    CHECK(func != nullptr);
    CHECK(parser->error_message().empty());

    CHECK(func->head.name() == "factorial");
    CHECK(func->head.param_count() == 1);
    CHECK(func->head.arg(0).name() == "input");
    CHECK(func->head.arg(0).type() == "int");
    CHECK(func->head.ret_type() == "int");
    CHECK(func->body != nullptr);

    auto * body = dynamic_cast<ast::stmt_sequence *>(func->body.get());
    CHECK(body != nullptr);
    CHECK(body->stmts.size() == 2);

    auto * if_stmt = dynamic_cast<ast::if_stmt *>(body->stmts[0].get());
    CHECK(if_stmt != nullptr);
    CHECK(if_stmt->condition != nullptr);
    CHECK(if_stmt->true_branch != nullptr);
    CHECK(if_stmt->else_branch == nullptr);
}

TEST_CASE("the parser will parse a module with one import") {
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

TEST_CASE("the parser will parse imports with or without semicolons") {
    std::string buffer = "from \"test.lil\" import foo, bar\nmain() {}";
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

TEST_CASE("the parser will parse a module with multiple imports") {
    std::string buffer = R"(from "test.lil" import foo, bar; from "foo.lil" import baz; main() {})";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);
    CHECK(parser->peek_token().first == parser::token_type::from);

    auto mod = parser->parse();
    CHECK(mod != nullptr);
    CHECK(parser->error_message().empty());
    std::cout << parser->error_message() << std::endl;

    CHECK(mod->imports.size() == 2);
    {
        auto iter = mod->imports.find("test.lil");
        CHECK_FALSE(iter == mod->imports.end());
        CHECK(iter->second.size() == 2);
    }
    {
        auto iter = mod->imports.find("foo.lil");
        CHECK_FALSE(iter == mod->imports.end());
        CHECK(iter->second.size() == 1);
    }

    CHECK(mod->items.size() == 1);
    CHECK_FALSE(mod->items.at(0) == nullptr);
}

TEST_CASE("the parser will parse a module with a single export") {
    std::string buffer = "export foo() {}";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto mod = parser->parse();
    CHECK(mod != nullptr);
    CHECK(parser->error_message().empty());

    CHECK(mod->imports.empty());

    CHECK(mod->items.size() == 1);
    CHECK_FALSE(mod->items[0] == nullptr);
    CHECK(mod->items[0]->exported());

    auto * func = dynamic_cast<ast::func_decl *>(mod->items[0].get());
    CHECK(func != nullptr);
    CHECK(func->head.name() == "foo");
}

TEST_CASE("the parser will parse a module with multiple exports") {
    std::string buffer = "export { foo() {}\nconst bar : int = 5 }";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto mod = parser->parse();
    CHECK(mod != nullptr);
    CHECK(parser->error_message().empty());

    CHECK(mod->imports.empty());

    CHECK(mod->items.size() == 2);
    CHECK_FALSE(mod->items[0] == nullptr);
    CHECK(mod->items[0]->exported());
    CHECK_FALSE(mod->items[1] == nullptr);
    CHECK(mod->items[1]->exported());

    auto * func = dynamic_cast<ast::func_decl *>(mod->items[0].get());
    CHECK(func != nullptr);
    CHECK(func->head.name() == "foo");

    auto * decl = dynamic_cast<ast::const_decl *>(mod->items[1].get());
    CHECK(decl != nullptr);
    CHECK(decl->name_and_type.name() == "bar");
}
