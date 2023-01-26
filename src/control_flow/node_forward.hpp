#pragma once

// clang-format off
#define all_cfg_nodes                    \
    expand_node_macro(binary_operation)  \
    expand_node_macro(branch)            \
    expand_node_macro(constant)          \
    expand_node_macro(function_call)     \
    expand_node_macro(function_end)      \
    expand_node_macro(function_start)    \
    expand_node_macro(intrinsic_call)    \
    expand_node_macro(phi)               \
    expand_node_macro(unary_operation)

// clang-format on

namespace control_flow {
    class node;

#define expand_node_macro(x) class x;
    all_cfg_nodes
#undef expand_node_macro
} // namespace control_flow
