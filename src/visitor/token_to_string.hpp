#ifndef TOKEN_TO_STRING_HPP
#define TOKEN_TO_STRING_HPP

#include <ast/expr_nodes.hpp>

#include <string>

[[nodiscard]] std::string tok_to_string(ast::binary_expr::operand);
[[nodiscard]] std::string tok_to_string(ast::unary_expr::operand);

#endif
