#include "token_to_string.hpp"

// TODO: Test this?
[[nodiscard]] std::string tok_to_string(ast::binary_expr::operand op) {
    using operand = ast::binary_expr::operand;

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
    }
}

[[nodiscard]] std::string tok_to_string(ast::unary_expr::operand op) {
    using operand = ast::unary_expr::operand;

    switch (op) {
    case operand::bool_not:
        return "operand::bool_not";
    case operand::negate:
        return "operand::negate";
    }
}
