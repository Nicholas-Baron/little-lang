#include "serializer.hpp"

#include "nodes.hpp"
#include "token_to_string.hpp"

#include <ostream>
#include <sstream>

namespace ast {

    void serializer::visit(binary_expr & binary_expr) {
        auto lhs = get_value(*binary_expr.lhs, *this);
        auto rhs = get_value(*binary_expr.rhs, *this);

        return store_result(std::map<std::string, nlohmann::json>{
            {"operator", nlohmann::json{tok_to_string(binary_expr.op)}},
            {"lhs", lhs},
            {"rhs", rhs}});
    }

    void serializer::visit(top_level_sequence & top_level_sequence) {
        std::vector<nlohmann::json> sequence;
        for (auto & top_level : top_level_sequence.items) {
            sequence.emplace_back(get_value(*top_level, *this));
        }

        return store_result(std::move(sequence));
    }

    // These four should not be called under any circumstance.
    // TODO: Remove from the trick macro.
    void serializer::visit(node & /*node*/) { assert(false and "Should not get here"); }
    void serializer::visit(expr & /*expr*/) { assert(false and "Should not get here"); }
    void serializer::visit(stmt & /*stmt*/) { assert(false and "Should not get here"); }
    void serializer::visit(top_level & /*top_level*/) { assert(false and "Should not get here"); }

    void serializer::visit(const_decl & const_decl) {
        auto value = get_value(*const_decl.expr, *this);

        return store_result(
            nlohmann::json::object_t{{"decl_type", "const"},
                                     {"variable", get_value(const_decl.name_and_type, *this)},
                                     {"value", std::move(value)}});
    }

    void serializer::visit(func_call_data & func_call_data) {

        std::vector<nlohmann::json> args(func_call_data.args_count());
        for (auto i = 0U; i < func_call_data.args_count(); ++i) {
            args[i] = get_value(func_call_data.arg(i), *this);
        }

        return store_result(std::map<std::string, nlohmann::json>{{"name", func_call_data.name()},
                                                                  {"args", std::move(args)}});
    }

    void serializer::visit(func_call_expr & func_call_expr) { visit(func_call_expr.data); }
    void serializer::visit(func_call_stmt & func_call_stmt) { visit(func_call_stmt.data); }

    void serializer::visit(func_decl & func_decl) {
        auto body = get_value(*func_decl.body, *this);

        std::vector<nlohmann::json> params;
        params.reserve(func_decl.head.param_count());

        for (auto & param : func_decl.head.params) { params.push_back(get_value(param, *this)); }

        assert(func_decl.head.ret_type() != nullptr);
        auto return_type = (std::stringstream{} << *func_decl.head.ret_type()).str();

        return store_result(nlohmann::json::object_t{{"body", std::move(body)},
                                                     {"name", func_decl.head.name()},
                                                     {"paramaters", std::move(params)},
                                                     {"return type", std::move(return_type)}});
    }

    void serializer::visit(ast::if_expr & if_expr) {
        auto condition = get_value(*if_expr.condition, *this);
        auto then_value = get_value(*if_expr.then_case, *this);
        auto else_value = get_value(*if_expr.else_case, *this);

        return store_result(
            std::map<std::string, nlohmann::json>{{"condition", std::move(condition)},
                                                  {"then_value", std::move(then_value)},
                                                  {"else_value", std::move(else_value)}});
    }

    void serializer::visit(ast::if_stmt & if_stmt) {

        auto condition = get_value(*if_stmt.condition, *this);
        auto then_value = get_value(*if_stmt.true_branch, *this);
        auto else_value = if_stmt.else_branch != nullptr ? get_value(*if_stmt.else_branch, *this)
                                                         : nlohmann::json{};

        return store_result(
            std::map<std::string, nlohmann::json>{{"condition", std::move(condition)},
                                                  {"then_value", std::move(then_value)},
                                                  {"else_value", std::move(else_value)}});
    }

    void serializer::visit(ast::let_stmt & let_stmt) {
        auto value = get_value(*let_stmt.value, *this);

        return store_result(
            nlohmann::json::object_t{{"decl_type", "let"},
                                     {"variable", get_value(let_stmt.name_and_type, *this)},
                                     {"value", std::move(value)}});
    }

    void serializer::visit(ast::return_stmt & return_stmt) {
        return store_result(std::map<std::string, nlohmann::json>{
            {"value",
             return_stmt.value != nullptr ? get_value(*return_stmt.value, *this) : nullptr}});
    }

    void serializer::visit(ast::stmt_sequence & stmt_sequence) {
        std::vector<nlohmann::json> stmts;

        for (auto & stmt : stmt_sequence.stmts) { stmts.push_back(get_value(*stmt, *this)); }

        return store_result(std::move(stmts));
    }

    void serializer::visit(ast::struct_decl & struct_decl) {

        std::vector<nlohmann::json> fields;

        for (auto & field : struct_decl.fields) { fields.push_back(get_value(field, *this)); }

        return store_result(
            nlohmann::json::object_t{{"name", struct_decl.name}, {"fields", std::move(fields)}});
    }

    void serializer::visit(ast::struct_init & struct_init) {

        std::vector<nlohmann::json> fields;

        for (auto & [name, value] : struct_init.initializers) {
            fields.emplace_back(
                nlohmann::json::object_t{{"name", name}, {"value", get_value(*value, *this)}});
        }

        return store_result({{"type", struct_init.type_name}, {"initializers", std::move(fields)}});
    }

    void serializer::visit(ast::typed_identifier & typed_identifier) {

        auto type = typed_identifier.type() != nullptr
                      ? nlohmann::json{(std::stringstream{} << typed_identifier.type()).str()}
                      : nlohmann::json{};

        return store_result(
            nlohmann::json::object_t{{"name", typed_identifier.name()}, {"type", std::move(type)}});
    }

    void serializer::visit(ast::unary_expr & unary_expr) {
        return store_result(
            {{"operator", unary_expr.op}, {"operand", get_value(*unary_expr.expr, *this)}});
    }

    void serializer::visit(ast::user_val & user_val) {
        return store_result(
            {{"value_type", tok_to_string(user_val.val_type)}, {"value", user_val.val}});
    }
} // namespace ast
