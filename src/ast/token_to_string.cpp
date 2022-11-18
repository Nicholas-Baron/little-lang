#include "token_to_string.hpp"

namespace ast {
    // TODO: Test this?
    // NOLINTNEXTLINE
    [[nodiscard]] std::string tok_to_string(binary_expr::operand op) {
        using operand = binary_expr::operand;

        switch (op) {
        case operand::eq:
            return "operand::eq";
        case operand::ne:
            return "operand::ne";
        case operand::lt:
            return "operand::lt";
        case operand::gt:
            return "operand::gt";
        case operand::le:
            return "operand::le";
        case operand::ge:
            return "operand::ge";
        case operand::add:
            return "operand::add";
        case operand::sub:
            return "operand::sub";
        case operand::div:
            return "operand::div";
        case operand::mult:
            return "operand::mult";
        case operand::mod:
            return "operand::mod";
        case operand::bool_and:
            return "operand::bool_and";
        case operand::bool_or:
            return "operand::bool_or";
        case operand::member_access:
            return "operand::member_access";
        }
    }

    // NOLINTNEXTLINE
    [[nodiscard]] std::string tok_to_string(unary_expr::operand op) {
        using operand = unary_expr::operand;

        switch (op) {
        case operand::bool_not:
            return "operand::bool_not";
        case operand::negate:
            return "operand::negate";
        case operand::deref:
            return "operand::deref";
        }
    }

    // NOLINTNEXTLINE
    [[nodiscard]] std::string tok_to_string(user_val::value_type typ) {
        using type = user_val::value_type;

        switch (typ) {
        case type::null:
            return "null";
        case type::identifier:
            return "identifier";
        case type::integer:
            return "int";
        case type::floating:
            return "float";
        case type::character:
            return "character";
        case type::boolean:
            return "boolean";
        case type::string:
            return "string";
        }
    }

} // namespace ast
