#ifndef VISITOR_BASE_HPP
#define VISITOR_BASE_HPP

#include <ast/nodes_forward.hpp>
#include <utils/move_copy.hpp>

namespace visitor {
    class visitor_base {
      public:
        non_copyable(visitor_base);

        movable(visitor_base);

        virtual ~visitor_base() = default;

        // clang-format off
#define __node(name) virtual void visit(ast::name &) = 0;
        ast_nodes
#undef __node

      protected:
        visitor_base() = default;
        // clang-format on
    };
} // namespace visitor

#endif
