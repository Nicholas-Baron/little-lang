
#ifndef VISITOR_HPP
#define VISITOR_HPP

#include "node_forward.hpp"

#include <move_copy.hpp>

namespace control_flow {

    class visitor {
      public:
        virtual ~visitor() noexcept = default;

        non_copyable(visitor);
        movable(visitor);

#define expand_node_macro(x) virtual void visit(x &) = 0;
        all_cfg_nodes
#undef expand_node_macro
    };

} // namespace control_flow
#endif
