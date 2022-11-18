#ifndef TOKEN_TO_STRING_HPP
#define TOKEN_TO_STRING_HPP

#include "expr_nodes.hpp"

#include <string>

[[nodiscard]] std::string tok_to_string(ast::binary_expr::operand /*op*/);
[[nodiscard]] std::string tok_to_string(ast::unary_expr::operand /*op*/);
[[nodiscard]] std::string tok_to_string(ast::user_val::value_type /*op*/);

#endif
