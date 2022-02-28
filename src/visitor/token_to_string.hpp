#ifndef TOKEN_TO_STRING_HPP
#define TOKEN_TO_STRING_HPP

#include <string>

#include <ast/expr_nodes.hpp>

[[nodiscard]] std::string tok_to_string(ast::binary_expr::operand /*op*/);
[[nodiscard]] std::string tok_to_string(ast::unary_expr::operand /*op*/);

#endif
