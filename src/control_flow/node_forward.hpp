#ifndef NODE_FORWARD_HPP
#define NODE_FORWARD_HPP

// clang-format off
#define all_cfg_nodes                    \
    expand_node_macro(branch)            \
    expand_node_macro(function_call)     \
    expand_node_macro(function_start)    \
    expand_node_macro(binary_operation)  \
    expand_node_macro(function_end)      \
    expand_node_macro(unary_operation)

// clang-format on

namespace control_flow {
#define expand_node_macro(x) class x;
    all_cfg_nodes
#undef expand_node_macro
} // namespace control_flow

#endif