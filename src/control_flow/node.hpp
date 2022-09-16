#ifndef NODE_HPP
#define NODE_HPP

#include "visitor.hpp"

#include <cstddef>
#include <vector>

#include <move_copy.hpp>

namespace control_flow {
    class node {
      public:
        non_copyable(node);
        movable(node);

        virtual ~node() noexcept = default;

        virtual void accept(visitor &) = 0;
    };

#define make_visitable \
    virtual void accept(visitor & visitor) override { visitor.visit(*this); }

    class function_start final : public node {
      public:
        make_visitable;

        // Invariant: cannot be null
        node * next;
        size_t arg_count{0};
        bool exported{false};
    };

    class binary_operation : public node {
      public:
        make_visitable;
    };

    class unary_operation : public node {
      public:
        make_visitable;
    };

    class function_call final : public node {
      public:
        make_visitable;

        // Invariant: none of the following `node *` may be null
        node * previous;
        node * next;
        function_start * callee;
        std::vector<node *> arguments;
    };

    // Handles both if expressions and if statements
    class branch final : public node {
      public:
        make_visitable;

        // Invariant: none of the following `node *` may be null
        node * previous;
        node * condition_value;
        node * true_case;
        node * false_case;
    };

    // Handles return statements and "fall off the end"
    class function_end final : public node {
      public:
        make_visitable;

        // Invariant: `previous` cannot be null
        node * previous;

        node * value;
    };

#undef make_visitable

} // namespace control_flow

#endif
