#pragma once

// clang-format off
#define ast_nodes                 \
    expand_node_macro(binary_expr)           \
    expand_node_macro(const_decl)            \
    expand_node_macro(expr)                  \
    expand_node_macro(func_call_data)        \
    expand_node_macro(func_call_expr)        \
    expand_node_macro(func_call_stmt)        \
    expand_node_macro(func_decl)             \
    expand_node_macro(if_expr)               \
    expand_node_macro(if_stmt)               \
    expand_node_macro(let_stmt)              \
    expand_node_macro(node)                  \
    expand_node_macro(return_stmt)           \
    expand_node_macro(stmt)                  \
    expand_node_macro(stmt_sequence)         \
    expand_node_macro(struct_decl)           \
    expand_node_macro(struct_init)           \
    expand_node_macro(top_level)             \
    expand_node_macro(top_level_sequence)    \
    expand_node_macro(typed_identifier)      \
    expand_node_macro(unary_expr)            \
    expand_node_macro(user_val)

// clang-format on

namespace ast {
#define expand_node_macro(name) class name;
    ast_nodes
#undef expand_node_macro
} // namespace ast
