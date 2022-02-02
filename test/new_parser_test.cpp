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

    CHECK(parser->peek_token() == parser::token_type::eof);

    CHECK(parser->parse() == nullptr);
    CHECK(parser->error_message() == "Found empty file");
}

TEST_CASE("the parser will report locations for tokens") {
    std::string buffer = "foo\n  bar";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token().location == Location{1, 0});
    CHECK(parser->next_token().location == Location{2, 2});
    CHECK(parser->next_token().location == Location{2, 5});

    CHECK(parser->error_message().empty());
}

TEST_CASE("the parser can look ahead for whole text fragments") {
    std::string buffer = "foo bar baz";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_chars("foo"));
    CHECK(parser->next_chars("oo", 1));
    CHECK(parser->next_chars("bar", 4));
    CHECK(parser->next_chars("baz", 8));
}

TEST_CASE("the parser will ignore comments") {
    std::string buffer = R"(// this is a comment
                         # this is another comment
                         comment I am from the 1960s
                         Comment I am also from the 1960s
                         foo bar baz)";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == "foo");
    CHECK(parser->next_token() == "bar");
    CHECK(parser->next_token() == "baz");
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse an identifier") {
    std::string buffer = "main";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto tok = parser->next_token();
    CHECK(tok == parser::token_type::identifier);
    CHECK(tok == "main");
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse primitive types") {
    std::string buffer = "int unit string char bool float";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::prim_type);
    CHECK(parser->next_token() == parser::token_type::prim_type);
    CHECK(parser->next_token() == parser::token_type::prim_type);
    CHECK(parser->next_token() == parser::token_type::prim_type);
    CHECK(parser->next_token() == parser::token_type::prim_type);
    CHECK(parser->next_token() == parser::token_type::prim_type);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse an identifier containing underscores") {
    std::string buffer = "my_value";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto tok = parser->next_token();
    CHECK(tok == parser::token_type::identifier);
    CHECK(tok == "my_value");
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse an identifier starting with underscores") {
    std::string buffer = "_value";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto tok = parser->next_token();
    CHECK(tok == parser::token_type::identifier);
    CHECK(tok == "_value");
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse a plain string") {
    std::string buffer = "\"raw\"";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto tok = parser->next_token();
    CHECK(tok == parser::token_type::string);
    CHECK(tok == "\"raw\"");
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse a character literal") {
    std::string buffer = "\'w\'";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto tok = parser->next_token();
    CHECK(tok == parser::token_type::character);
    CHECK(tok == "\'w\'");
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse an integer") {
    std::string buffer = "1234";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto tok = parser->next_token();
    CHECK(tok == parser::token_type::integer);
    CHECK(tok == "1234");
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse a hexadecimal integer") {
    std::string buffer = "0x123456789aBcDeF";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto tok = parser->next_token();
    CHECK(tok == parser::token_type::integer);
    CHECK(tok == buffer);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse a colon and the word 'is' as the same token") {
    std::string buffer = "is:";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::colon);
    CHECK(parser->next_token() == parser::token_type::colon);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse parentheses") {
    std::string buffer = "()";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::lparen);
    CHECK(parser->next_token() == parser::token_type::rparen);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse braces") {
    std::string buffer = "{}";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::lbrace);
    CHECK(parser->next_token() == parser::token_type::rbrace);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse a 'skinny' arrow") {
    std::string buffer = "->";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::arrow);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse a comma") {
    std::string buffer = ",";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::comma);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse a semicolon") {
    std::string buffer = ";";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::semi);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse 'from', 'import', and 'export'") {
    std::string buffer = "from import export";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::from);
    CHECK(parser->next_token() == parser::token_type::import_);
    CHECK(parser->next_token() == parser::token_type::export_);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse 'if', 'then', and 'else'") {
    std::string buffer = "if else then";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::if_);
    CHECK(parser->next_token() == parser::token_type::else_);
    CHECK(parser->next_token() == parser::token_type::then);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse 'return' and 'ret' the same") {
    std::string buffer = "return ret";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::return_);
    CHECK(parser->next_token() == parser::token_type::return_);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse '<' and '>'") {
    std::string buffer = "< >";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::lt);
    CHECK(parser->next_token() == parser::token_type::gt);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse '<=' and '>=' as 1 token each") {
    std::string buffer = "<= >=";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::le);
    CHECK(parser->next_token() == parser::token_type::ge);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse 'let' and 'const'") {
    std::string buffer = "let const";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::let);
    CHECK(parser->next_token() == parser::token_type::const_);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse 'and' as '&&'") {
    std::string buffer = "and &&";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::double_and);
    CHECK(parser->next_token() == parser::token_type::double_and);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse 'or' as '||'") {
    std::string buffer = "or ||";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::double_or);
    CHECK(parser->next_token() == parser::token_type::double_or);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse 'equals' as '=='") {
    std::string buffer = "equals ==";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::eq);
    CHECK(parser->next_token() == parser::token_type::eq);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse basic mathematical symbols") {
    std::string buffer = "= + - / * %";

    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->next_token() == parser::token_type::equal);
    CHECK(parser->next_token() == parser::token_type::plus);
    CHECK(parser->next_token() == parser::token_type::minus);
    CHECK(parser->next_token() == parser::token_type::slash);
    CHECK(parser->next_token() == parser::token_type::asterik);
    CHECK(parser->next_token() == parser::token_type::percent);
    CHECK(parser->next_token() == parser::token_type::eof);
}

TEST_CASE("the parser will parse pointer-related tokens") {
    std::string buffer = " & ? null";

    auto parser = parser::from_buffer(buffer);
    CHECK(parser != nullptr);
    CHECK(parser->next_token() == parser::token_type::amp);
    CHECK(parser->next_token() == parser::token_type::question);
    CHECK(parser->next_token() == parser::token_type::null);
    CHECK(parser->next_token() == parser::token_type::eof);
}

// ast level
TEST_CASE("the parser will parse braces as a compound statement") {
    std::string buffer = "{}";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_compound_statement();
    CHECK(stmt != nullptr);
}

TEST_CASE("the parser will parse typed identifiers in both new style and C-style") {

    ast::typed_identifier typed_id_1 = [] {
        std::string buffer = "int x";
        auto parser = parser::from_buffer(buffer);

        CHECK(parser != nullptr);

        return parser->parse_typed_identifier();
    }();

    ast::typed_identifier typed_id_2 = [] {
        std::string buffer = "x : int";
        auto parser = parser::from_buffer(buffer);

        CHECK(parser != nullptr);

        return parser->parse_typed_identifier();
    }();

    CHECK(typed_id_1.name() == typed_id_2.name());
    CHECK(*typed_id_1.type() == *typed_id_2.type());
}

TEST_CASE("the parser will parse optionally typed identifiers") {

    ast::typed_identifier typed_id_1 = [] {
        std::string buffer = "x";
        auto parser = parser::from_buffer(buffer);

        CHECK(parser != nullptr);

        return parser->parse_opt_typed_identifier();
    }();

    ast::typed_identifier typed_id_2 = [] {
        std::string buffer = "x : int";
        auto parser = parser::from_buffer(buffer);

        CHECK(parser != nullptr);

        return parser->parse_opt_typed_identifier();
    }();

    ast::typed_identifier typed_id_3 = [] {
        std::string buffer = "int x";
        auto parser = parser::from_buffer(buffer);

        CHECK(parser != nullptr);

        return parser->parse_opt_typed_identifier();
    }();

    CHECK(typed_id_1.name() == typed_id_2.name());
    CHECK(typed_id_1.name() == typed_id_3.name());
}

TEST_CASE("the parser will parse let statement") {
    std::string buffer = "let x = \"hello\";";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    CHECK(parser->peek_token() == parser::token_type::eof);

    auto * let = dynamic_cast<ast::let_stmt *>(stmt.get());
    CHECK(let != nullptr);
    CHECK(let->name_and_type.name() == "x");
    CHECK(let->name_and_type.type() == nullptr);
    CHECK(let->value != nullptr);

    auto * value = dynamic_cast<ast::user_val *>(let->value.get());
    CHECK(value != nullptr);
    CHECK(value->val_type == ast::user_val::value_type::string);
    CHECK(value->val == "\"hello\"");
}

TEST_CASE("the parser will parse let statement without semicolons and with types") {
    std::string buffer = "let int x = 10";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    CHECK(parser->peek_token() == parser::token_type::eof);

    auto * let = dynamic_cast<ast::let_stmt *>(stmt.get());
    CHECK(let != nullptr);
    CHECK(let->name_and_type.name() == "x");
    CHECK(*let->name_and_type.type() == *ast::prim_type::int32);
    CHECK(let->value != nullptr);

    auto * value = dynamic_cast<ast::user_val *>(let->value.get());
    CHECK(value != nullptr);
    CHECK(value->val_type == ast::user_val::value_type::integer);
    CHECK(value->val == "10");
}

TEST_CASE("the parser will parse pointer types and expressions") {
    std::string buffer = "let ? int x = null;";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    CHECK(parser->peek_token() == parser::token_type::eof);

    auto * let = dynamic_cast<ast::let_stmt *>(stmt.get());
    CHECK(let != nullptr);
    CHECK(let->name_and_type.name() == "x");
    CHECK(*let->name_and_type.type()
          == *std::make_shared<ast::nullable_ptr_type>(ast::prim_type::int32));
    CHECK(let->value != nullptr);

    auto * value = dynamic_cast<ast::user_val *>(let->value.get());
    CHECK(value != nullptr);
    CHECK(value->val_type == ast::user_val::value_type::null);
    CHECK(value->val == "null");
}

TEST_CASE("the parser will parse dereferences") {
    std::string buffer = "let x = *y";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    CHECK(parser->peek_token() == parser::token_type::eof);

    auto * let = dynamic_cast<ast::let_stmt *>(stmt.get());
    CHECK(let != nullptr);
    CHECK(let->name_and_type.name() == "x");
    CHECK(let->value != nullptr);

    auto * value = dynamic_cast<ast::unary_expr *>(let->value.get());
    CHECK(value != nullptr);
    CHECK(value->op == ast::unary_expr::operand::deref);
    CHECK(value->expr != nullptr);
}

TEST_CASE("the parser will parse unary minus") {
    std::string buffer = "-3";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto expr = parser->parse_expression();
    CHECK(expr != nullptr);
    CHECK(parser->peek_token() == parser::token_type::eof);

    auto * unary_expr = dynamic_cast<ast::unary_expr *>(expr.get());
    CHECK(unary_expr != nullptr);
    CHECK(unary_expr->expr != nullptr);
    CHECK(unary_expr->op == ast::unary_expr::operand::negate);

    auto * val = dynamic_cast<ast::user_val *>(unary_expr->expr.get());
    CHECK(val != nullptr);
    CHECK(val->val_type == ast::user_val::value_type::integer);
    CHECK(val->val == "3");
}

TEST_CASE("the parser will parse const declaration") {
    std::string buffer = "const x : int = 5 * -3;";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto decl = parser->parse_top_level();
    CHECK(decl != nullptr);
    CHECK(parser->peek_token() == parser::token_type::eof);

    auto * const_decl = dynamic_cast<ast::const_decl *>(decl.get());
    CHECK(const_decl != nullptr);
    CHECK(const_decl->name_and_type.name() == "x");
    CHECK(*const_decl->name_and_type.type() == *ast::prim_type::int32);
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

TEST_CASE("the parser will parse if expressions") {
    std::string buffer = " if x then y else z + 1 ";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    auto expr = parser->parse_expression();
    CHECK(expr != nullptr);

    auto * if_expr = dynamic_cast<ast::if_expr *>(expr.get());
    CHECK(if_expr != nullptr);
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
    CHECK(cond->val_type == ast::user_val::value_type::identifier);
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
    CHECK(cond->val_type == ast::user_val::value_type::identifier);
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
}

TEST_CASE("the parser will parse a unit function") {
    std::string buffer = "main() {}";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);

    CHECK(parser->peek_token() == parser::token_type::identifier);

    auto func = parser->parse_function();
    CHECK(func != nullptr);
    CHECK(parser->error_message().empty());

    CHECK(func->head.name() == "main");
    CHECK(func->head.param_count() == 0);
    CHECK(*func->head.ret_type() == *ast::prim_type::unit);
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
    CHECK(*func->head.ret_type() == *ast::prim_type::int32);
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
    CHECK(*func->head.arg(0).type() == *ast::prim_type::int32);
    CHECK(func->head.arg(1).name() == "y");
    CHECK(*func->head.arg(1).type() == *ast::prim_type::boolean);
    CHECK(*func->head.ret_type() == *ast::prim_type::int32);
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
    CHECK(*func->head.ret_type() == *ast::prim_type::int32);
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
    CHECK(*func->head.arg(0).type() == *ast::prim_type::int32);
    CHECK(*func->head.ret_type() == *ast::prim_type::int32);
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
    CHECK(parser->peek_token() == parser::token_type::from);

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
    CHECK(parser->peek_token() == parser::token_type::from);

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
    CHECK(parser->peek_token() == parser::token_type::from);

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
