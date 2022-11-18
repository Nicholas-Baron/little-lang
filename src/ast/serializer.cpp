#include "serializer.hpp"

#include "nodes.hpp"
#include "token_to_string.hpp"

#include <ostream>
#include <sstream>

namespace ast {
    serializer::serializer(const std::string & filename) { mod.emplace("filename", filename); }

    void serializer::dump(std::ostream & stream, bool human_readable) const {
        stream << mod.dump(human_readable ? 4 : -1);
    }

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

        auto type = (std::stringstream{} << const_decl.name_and_type.type()).str();

        return store_result(
            std::map<std::string, nlohmann::json>{{"name", const_decl.name_and_type.name()},
                                                  {"type", std::move(type)},
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

        std::vector<nlohmann::json> params(func_decl.head.param_count());
        for (auto i = 0U; i < func_decl.head.param_count(); ++i) {
            auto type = (std::stringstream{} << func_decl.head.arg(i).type()).str();
            params[i] = std::map<std::string, nlohmann::json>{
                {"name", func_decl.head.arg(i).name()}, {"type", std::move(type)}};
        }

        return store_result(std::map<std::string, nlohmann::json>{
            {"body", std::move(body)},
            {"name", func_decl.head.name()},
            {"paramaters", std::move(params)},
            {"return type", (std::stringstream{} << func_decl.head.ret_type()).str()}});
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

        // TODO: Allow `typed_identifier` to be visited
        auto type = (std::stringstream{} << let_stmt.name_and_type.type()).str();

        return store_result(
            std::map<std::string, nlohmann::json>{{"type", std::move(type)},
                                                  {"name", let_stmt.name_and_type.name()},
                                                  {"value", std::move(value)}});
    }
} // namespace ast
