#ifndef GRAPH_HPP
#define GRAPH_HPP

#include <move_copy.hpp>

namespace control_flow {
    class visitor;

    class node {
      public:
        non_copyable(node);
        movable(node);

        virtual ~node() noexcept = default;

        virtual void accept(visitor &) const = 0;
    };
} // namespace control_flow

#endif
