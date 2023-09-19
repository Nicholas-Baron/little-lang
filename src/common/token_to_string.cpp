#include "token_to_string.hpp"

#include "operations.hpp"

// TODO: Test this?
[[nodiscard]] std::string token_to_string(operation::binary operation) {
    using operand = operation::binary;

    switch (operation) {
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

[[nodiscard]] std::string token_to_string(operation::unary operation) {
    using operand = operation::unary;

    switch (operation) {
    case operand::bool_not:
        return "operand::bool_not";
    case operand::negate:
        return "operand::negate";
    case operand::deref:
        return "operand::deref";
    case operand::addrof:
        return "operand::addrof";
    }
}

[[nodiscard]] std::string token_to_string(literal_type typ) {
    using type = literal_type;

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
