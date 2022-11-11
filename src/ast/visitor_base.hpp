#ifndef VISITOR_BASE_HPP
#define VISITOR_BASE_HPP

#include "nodes_forward.hpp"

#include <move_copy.hpp>

namespace ast {
    class visitor_base {
      public:
        non_copyable(visitor_base);

        movable(visitor_base);

        virtual ~visitor_base() = default;

        // clang-format off
#define expand_node_macro(name) virtual void visit(ast::name &) = 0;
        ast_nodes
#undef expand_node_macro

      protected:
        visitor_base() = default;
        // clang-format on
    };
} // namespace ast

#endif
