#ifndef NODES_FORWARD_HPP
#define NODES_FORWARD_HPP

// clang-format off
#define ast_nodes                 \
    __node(binary_expr)           \
    __node(const_decl)            \
    __node(expr)                  \
    __node(func_call_data)        \
    __node(func_call_expr)        \
    __node(func_call_stmt)        \
    __node(func_decl)             \
    __node(func_header)           \
    __node(if_stmt)               \
    __node(let_stmt)              \
    __node(node)                  \
    __node(return_stmt)           \
    __node(stmt)                  \
    __node(stmt_sequence)         \
    __node(top_level)             \
    __node(top_level_sequence)    \
    __node(typed_identifier)      \
    __node(unary_expr)            \
    __node(user_val)

// clang-format on

namespace ast {
#define __node(name) class name;
    ast_nodes
#undef __node
} // namespace ast

#endif
