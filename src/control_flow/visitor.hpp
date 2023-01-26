
#pragma once

#include "node_forward.hpp"

#include <move_copy.hpp>

namespace control_flow {

    class visitor {
      public:
        virtual ~visitor() noexcept = default;

        non_copyable(visitor);
        movable(visitor);

        // clang-format off
#define expand_node_macro(x) virtual void visit(x &) = 0;
        all_cfg_nodes
#undef expand_node_macro

        void visit(control_flow::node & node);
        // clang-format on

      protected:
        visitor() = default;
    };

} // namespace control_flow
