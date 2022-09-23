#ifndef NODE_HPP
#define NODE_HPP

#include "visitor.hpp"

#include <string>
#include <variant>
#include <vector>

#include <move_copy.hpp>

namespace control_flow {
#undef make_visitable

    class node {
      public:
        non_copyable(node);
        movable(node);

        virtual ~node() noexcept = default;

        virtual void accept(visitor &) = 0;

      protected:
        node() = default;
    };

#define make_visitable \
    virtual void accept(visitor & visitor) override { visitor.visit(*this); }

    class function_start final : public node {
      public:
        function_start(size_t arg_count, bool exported)
            : arg_count{arg_count}
            , exported{exported} {}

        make_visitable;

        // Invariant: cannot be null
        node * next{nullptr};
        size_t arg_count;
        bool exported;
    };

    class binary_operation final : public node {
      public:
        enum class operand {
            add,
            sub,
            mult,
            div,
            mod,
            gt,
            ge,
            lt,
            le,
            eq,
            ne,
            bool_and,
            bool_or,
            member_access,
        };

        make_visitable;

        // Invariant: none of the following `node *` may be null
        node * previous;
        node * next;
        node * lhs;
        node * rhs;
    };

    class unary_operation final : public node {
      public:
        enum class operation { bool_not, negate, deref };

        make_visitable;

        // Invariant: none of the following `node *` may be null
        node * previous;
        node * next;
        node * operand;
    };

    // Constants do not sit in the control flow path, so they do not need a `previous` or `next`.
    // TODO: This will not be the case when mutability is added.
    class constant final : public node {
      public:
        enum class value_type { null, identifier, integer, floating, character, boolean, string };

        make_visitable;

        std::variant<std::monostate, long, double, char, bool, std::string> value;
        value_type val_type;
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
