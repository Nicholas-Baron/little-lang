#include "serializer.hpp"

#include "nodes.hpp"
#include "token_to_string.hpp"

#include <ostream>

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
} // namespace ast
