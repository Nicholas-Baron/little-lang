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
} // namespace ast
