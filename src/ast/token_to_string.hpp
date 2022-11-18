#ifndef TOKEN_TO_STRING_HPP
#define TOKEN_TO_STRING_HPP

#include "expr_nodes.hpp"

#include <string>

namespace ast {
    [[nodiscard]] std::string tok_to_string(binary_expr::operand /*op*/);
    [[nodiscard]] std::string tok_to_string(unary_expr::operand /*op*/);
    [[nodiscard]] std::string tok_to_string(user_val::value_type /*op*/);
} // namespace ast

#endif
