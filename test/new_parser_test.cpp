#include "ast/nodes.hpp"

#define PARSER_TEST
#include "ast/parser.hpp"

#include <catch2/catch.hpp>

TEST_CASE("the parser will parse braces as a compound statement") {
    std::string buffer = "{}";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_compound_statement();
    CHECK(stmt != nullptr);
}

TEST_CASE("the parser will parse typed identifiers in both new style and C-style") {

    ast::type_context ty_context;

    ast::typed_identifier typed_id_1 = [&ty_context] {
        std::string buffer = "int8 x";
        auto parser = parser::from_buffer(buffer, ty_context);

        CHECK(parser != nullptr);

        return parser->parse_typed_identifier();
    }();

    ast::typed_identifier typed_id_2 = [&ty_context] {
        std::string buffer = "x : int8";
        auto parser = parser::from_buffer(buffer, ty_context);

        CHECK(parser != nullptr);

        return parser->parse_typed_identifier();
    }();

    CHECK(typed_id_1.name() == typed_id_2.name());
    CHECK(typed_id_1.type() == typed_id_2.type());
}

TEST_CASE("the parser will parse optionally typed identifiers") {

    ast::typed_identifier typed_id_1 = [] {
        std::string buffer = "x";
        ast::type_context ty_context;
        auto parser = parser::from_buffer(buffer, ty_context);

        CHECK(parser != nullptr);

        return parser->parse_opt_typed_identifier();
    }();

    ast::typed_identifier typed_id_2 = [] {
        std::string buffer = "x : int8";
        ast::type_context ty_context;
        auto parser = parser::from_buffer(buffer, ty_context);

        CHECK(parser != nullptr);

        return parser->parse_opt_typed_identifier();
    }();

    ast::typed_identifier typed_id_3 = [] {
        std::string buffer = "int8 x";
        ast::type_context ty_context;
        auto parser = parser::from_buffer(buffer, ty_context);

        CHECK(parser != nullptr);

        return parser->parse_opt_typed_identifier();
    }();

    CHECK(typed_id_1.name() == typed_id_2.name());
    CHECK(typed_id_1.name() == typed_id_3.name());
}

TEST_CASE("the parser will parse let statement") {
    std::string buffer = "let x = \"hello\";";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    CHECK(parser->is_eof());

    auto * let = dynamic_cast<ast::let_stmt *>(stmt.get());
    CHECK(let != nullptr);
    CHECK(let->name_and_type.name() == "x");
    CHECK(let->name_and_type.type() == nullptr);
    CHECK(let->value != nullptr);

    auto * value = dynamic_cast<ast::user_val *>(let->value.get());
    CHECK(value != nullptr);
    CHECK(value->val_type == literal_type::string);
    CHECK(value->val == "\"hello\"");
}

TEST_CASE("the parser will parse let statement with types") {
    std::string buffer = "let int32 x = 10;";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    CHECK(parser->is_eof());

    auto * let = dynamic_cast<ast::let_stmt *>(stmt.get());
    CHECK(let != nullptr);
    CHECK(let->name_and_type.name() == "x");
    CHECK(let->name_and_type.type() == ty_context.create_type<ast::int_type>(32));
    CHECK(let->value != nullptr);

    auto * value = dynamic_cast<ast::user_val *>(let->value.get());
    CHECK(value != nullptr);
    CHECK(value->val_type == literal_type::integer);
    CHECK(value->val == "10");
}

TEST_CASE("the parser will parse pointer types and expressions") {
    std::string buffer = "let ? int64 x = null;";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    CHECK(parser->is_eof());

    auto * let = dynamic_cast<ast::let_stmt *>(stmt.get());
    CHECK(let != nullptr);
    CHECK(let->name_and_type.name() == "x");

    auto * int_type = ty_context.create_type<ast::int_type>(64);
    CHECK(let->name_and_type.type() == ty_context.create_type<ast::nullable_ptr_type>(int_type));
    CHECK(let->value != nullptr);

    auto * value = dynamic_cast<ast::user_val *>(let->value.get());
    CHECK(value != nullptr);
    CHECK(value->val_type == literal_type::null);
    CHECK(value->val == "null");
}

TEST_CASE("the parser will parse dereferences") {
    std::string buffer = "let x = *y;";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    CHECK(parser->is_eof());

    auto * let = dynamic_cast<ast::let_stmt *>(stmt.get());
    CHECK(let != nullptr);
    CHECK(let->name_and_type.name() == "x");
    CHECK(let->value != nullptr);

    auto * value = dynamic_cast<ast::unary_expr *>(let->value.get());
    CHECK(value != nullptr);
    CHECK(value->op == operation::unary::deref);
    CHECK(value->expr != nullptr);
}

TEST_CASE("the parser will parse unary minus") {
    std::string buffer = "-3";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto expr = parser->parse_expression();
    CHECK(expr != nullptr);
    CHECK(parser->is_eof());

    auto * unary_expr = dynamic_cast<ast::unary_expr *>(expr.get());
    CHECK(unary_expr != nullptr);
    CHECK(unary_expr->expr != nullptr);
    CHECK(unary_expr->op == operation::unary::negate);

    auto * val = dynamic_cast<ast::user_val *>(unary_expr->expr.get());
    CHECK(val != nullptr);
    CHECK(val->val_type == literal_type::integer);
    CHECK(val->val == "3");
}

TEST_CASE("the parser will parse type casts") {
    std::string buffer = "-x as int32";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    auto expr = parser->parse_expression();
    CHECK(expr != nullptr);
    CHECK(parser->is_eof());

    auto * cast_expr = dynamic_cast<ast::cast_expr *>(expr.get());
    CHECK(cast_expr != nullptr);
    CHECK(cast_expr->destination != nullptr);

    auto * unary_expr = dynamic_cast<ast::unary_expr *>(cast_expr->operand.get());
    CHECK(unary_expr != nullptr);
    CHECK(unary_expr->expr != nullptr);
    CHECK(unary_expr->op == operation::unary::negate);
}

TEST_CASE("the parser will parse const declaration") {
    std::string buffer = "const x : int16 = 5 * -3;";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto decl = parser->parse_top_level();
    CHECK(decl != nullptr);
    CHECK(parser->is_eof());

    auto * const_decl = dynamic_cast<ast::const_decl *>(decl.get());
    CHECK(const_decl != nullptr);
    CHECK(const_decl->name_and_type.name() == "x");
    CHECK(const_decl->name_and_type.type() == ty_context.create_type<ast::int_type>(16));
    CHECK(const_decl->expr != nullptr);

    auto * value = dynamic_cast<ast::binary_expr *>(const_decl->expr.get());
    CHECK(value != nullptr);
    CHECK(value->op == operation::binary::mult);
    CHECK(value->lhs != nullptr);
    CHECK(value->rhs != nullptr);
}

TEST_CASE("the parser will parse binary expressions") {
    std::string buffer = " 5 + 10 ";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_expression();
    CHECK(stmt != nullptr);
}

TEST_CASE("the parser will parse if expressions") {
    std::string buffer = " if x then y else z + 1 ";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto expr = parser->parse_expression();
    CHECK(expr != nullptr);

    auto * if_expr = dynamic_cast<ast::if_expr *>(expr.get());
    CHECK(if_expr != nullptr);
}

TEST_CASE("the parser will parse if expressions as atoms") {
    std::string buffer = " 1 + if x then y else z + 1 ";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto expr = parser->parse_expression();
    CHECK(expr != nullptr);

    auto * outer_addition = dynamic_cast<ast::binary_expr *>(expr.get());
    CHECK(outer_addition != nullptr);

    auto * if_expr = dynamic_cast<ast::if_expr *>(outer_addition->rhs.get());
    CHECK(if_expr != nullptr);
}

TEST_CASE("the parser will parse if statements") {
    std::string buffer = "if x then {}";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    auto * if_stmt = dynamic_cast<ast::if_stmt *>(stmt.get());
    CHECK(if_stmt != nullptr);
    CHECK(if_stmt->else_branch == nullptr);
    CHECK(if_stmt->true_branch != nullptr);

    auto * cond = dynamic_cast<ast::user_val *>(if_stmt->condition.get());
    CHECK(cond != nullptr);
    CHECK(cond->val_type == literal_type::identifier);
    CHECK(cond->val == "x");
}

TEST_CASE("the parser will parse if-else statements") {
    std::string buffer = "if x then {} else {}";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    auto * if_stmt = dynamic_cast<ast::if_stmt *>(stmt.get());
    CHECK(if_stmt != nullptr);
    CHECK(if_stmt->else_branch != nullptr);
    CHECK(if_stmt->true_branch != nullptr);

    auto * cond = dynamic_cast<ast::user_val *>(if_stmt->condition.get());
    CHECK(cond != nullptr);
    CHECK(cond->val_type == literal_type::identifier);
    CHECK(cond->val == "x");
}

TEST_CASE("the parser will parse return statements") {
    std::string buffer = "return;";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    auto * ret_stmt = dynamic_cast<ast::return_stmt *>(stmt.get());
    CHECK(ret_stmt != nullptr);
    CHECK(ret_stmt->value == nullptr);
}

TEST_CASE("the parser will parse return statements with values") {
    std::string buffer = "return 5 * 10;";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto stmt = parser->parse_statement();
    CHECK(stmt != nullptr);
    auto * ret_stmt = dynamic_cast<ast::return_stmt *>(stmt.get());
    CHECK(ret_stmt != nullptr);
    CHECK(ret_stmt->value != nullptr);

    auto * value = dynamic_cast<ast::binary_expr *>(ret_stmt->value.get());
    CHECK(value != nullptr);
    CHECK(value->op == operation::binary::mult);
    CHECK(value->lhs != nullptr);
    CHECK(value->rhs != nullptr);
}

TEST_CASE("the parser will parse function calls as expressions") {
    std::string buffer = "return foo(5, 'x');";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

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
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

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

TEST_CASE("the parser will parse a struct declaration") {
    std::string buffer = R"(my_struct_type {
		x : int8;
		y : string,
		z : bool
	})";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto top_lvl = parser->parse_top_level();
    CHECK(parser->error_message().empty());

    auto * struct_decl = dynamic_cast<ast::struct_decl *>(top_lvl.get());
    CHECK(struct_decl != nullptr);

    CHECK(struct_decl->name == "my_struct_type");
    CHECK(struct_decl->fields.size() == 3);
    CHECK(struct_decl->fields[0].name() == "x");
}

TEST_CASE("the parser will parse a unit function") {
    std::string buffer = "main() {}";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto func = parser->parse_function();
    CHECK(func != nullptr);
    CHECK(parser->error_message().empty());

    CHECK(func->name == "main");
    CHECK(func->param_count() == 0);
    CHECK(func->func_type->return_type()
          == ty_context.create_type<ast::prim_type>(ast::prim_type::type::unit));
    CHECK(func->body != nullptr);
}

TEST_CASE("the parser will parse a struct initialization") {
    std::string buffer = "let x = my_struct_type { x = 5; y = 6, z = 10, a = 6 / 2 };";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto init = parser->parse_let_statement();
    CHECK(init != nullptr);
    CHECK(parser->error_message().empty());

    auto * struct_init = dynamic_cast<ast::struct_init *>(init->value.get());
    CHECK(struct_init != nullptr);

    CHECK(struct_init->type_name == "my_struct_type");
    CHECK(struct_init->initializers.size() == 4);
    CHECK(struct_init->initializers[0].first == "x");
    CHECK(struct_init->initializers[1].first == "y");
    CHECK(struct_init->initializers[2].first == "z");
    CHECK(struct_init->initializers[3].first == "a");
}

TEST_CASE("the parser will parse a member access") {
    std::string buffer = "x.y.z";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto expr = parser->parse_expression();
    CHECK(expr != nullptr);
    CHECK(parser->error_message().empty());

    auto * outer_member_access = dynamic_cast<ast::binary_expr *>(expr.get());
    CHECK(outer_member_access != nullptr);
    CHECK(outer_member_access->op == operation::binary::member_access);
    CHECK(outer_member_access->lhs != nullptr);
    CHECK(outer_member_access->rhs != nullptr);

    auto * rhs = dynamic_cast<ast::user_val *>(outer_member_access->rhs.get());
    CHECK(rhs != nullptr);
    CHECK(rhs->val_type == literal_type::identifier);
    CHECK(rhs->val == "z");

    auto * inner_member_access = dynamic_cast<ast::binary_expr *>(outer_member_access->lhs.get());
    CHECK(inner_member_access != nullptr);
    CHECK(inner_member_access->op == operation::binary::member_access);
    CHECK(inner_member_access->lhs != nullptr);
    CHECK(inner_member_access->rhs != nullptr);
}

TEST_CASE("the parser will parse a function with return type") {
    std::string buffer = "main() -> int32 {}";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto func = parser->parse_function();
    CHECK(func != nullptr);
    CHECK(parser->error_message().empty());

    CHECK(func->name == "main");
    CHECK(func->param_count() == 0);
    CHECK(func->func_type->return_type() == ty_context.create_type<ast::int_type>(32));
    CHECK(func->body != nullptr);
}

TEST_CASE("the parser will parse a function with parameters") {
    std::string buffer = "foo(int64 x, y : bool) -> int64 {}";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto func = parser->parse_function();
    CHECK(func != nullptr);
    CHECK(parser->error_message().empty());

    CHECK(func->name == "foo");
    CHECK(func->param_count() == 2);
    CHECK(func->params[0].name() == "x");
    CHECK(func->params[0].type() == ty_context.create_type<ast::int_type>(64));
    CHECK(func->params[1].name() == "y");
    CHECK(func->params[1].type()
          == ty_context.create_type<ast::prim_type>(ast::prim_type::type::boolean));
    CHECK(func->func_type->return_type() == ty_context.create_type<ast::int_type>(64));
    CHECK(func->body != nullptr);
}

TEST_CASE("the parser will parse a function with an expression body") {
    std::string buffer = "foo(int32 x, y : int32) -> int32 = (x + y) / 2";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto func = parser->parse_function();
    CHECK(func != nullptr);
    CHECK(parser->error_message().empty());

    CHECK(func->name == "foo");
    CHECK(func->param_count() == 2);
    CHECK(func->func_type->return_type() == ty_context.create_type<ast::int_type>(32));
    CHECK(func->body != nullptr);

    auto * ret_stmt = dynamic_cast<ast::return_stmt *>(func->body.get());
    CHECK(ret_stmt != nullptr);
    CHECK(ret_stmt->value != nullptr);

    auto * value = dynamic_cast<ast::binary_expr *>(ret_stmt->value.get());
    CHECK(value != nullptr);
    CHECK(value->lhs != nullptr);
    CHECK(value->rhs != nullptr);
    CHECK(value->op == operation::binary::div);
}

TEST_CASE("the parser will parse a factorial function") {
    std::string buffer = R"(
factorial(int8 input) -> int64 {
	if input <= 2 then { return input; }
	return input * factorial(input - 1);
})";

    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto func = parser->parse_function();
    CHECK(func != nullptr);
    CHECK(parser->error_message().empty());

    CHECK(func->name == "factorial");
    CHECK(func->param_count() == 1);
    CHECK(func->params[0].name() == "input");
    CHECK(func->params[0].type() == ty_context.create_type<ast::int_type>(8));
    CHECK(func->func_type->return_type() == ty_context.create_type<ast::int_type>(64));
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
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto mod = parser->parse();
    CHECK(mod != nullptr);
    CHECK(parser->error_message().empty());

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
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto mod = parser->parse();
    CHECK(mod != nullptr);
    CHECK(parser->error_message().empty());

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
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

    CHECK(parser != nullptr);

    auto mod = parser->parse();
    CHECK(mod != nullptr);
    CHECK(parser->error_message().empty());

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
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

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
    CHECK(func->name == "foo");
}

TEST_CASE("the parser will parse a module with multiple exports") {
    std::string buffer = "export { foo() {}\nconst bar : int32 = 5 }";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);

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
    CHECK(func->name == "foo");

    auto * decl = dynamic_cast<ast::const_decl *>(mod->items[1].get());
    CHECK(decl != nullptr);
    CHECK(decl->name_and_type.name() == "bar");
}
