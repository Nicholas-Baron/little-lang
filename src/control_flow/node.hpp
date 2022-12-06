#ifndef NODE_HPP
#define NODE_HPP

#include "visitor.hpp"

#include <cassert>
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

        // Makes `node` the previous of `this`.
        virtual void flows_from(node * node) = 0;

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

        void flows_from(node * /*node*/) override {
            assert(false and "There is no previous to a function_start");
        }

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

        void flows_from(node * node) override { previous = node; }

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

        void flows_from(node * node) override { previous = node; }

        // Invariant: none of the following `node *` may be null
        node * previous;
        node * next;
        node * operand;
        operation op;
    };

    // TODO: This will not be the case when mutability is added.
    class constant final : public node {
      public:
        enum class value_type { null, identifier, integer, floating, character, boolean, string };

        make_visitable;

        void flows_from(node * node) override { previous = node; }

        std::variant<std::monostate, long, double, char, bool, std::string> value;
        value_type val_type;

        // Invariant: none of the following `node *` may be null
        node * previous{nullptr};
        node * next{nullptr};
    };

    class function_call final : public node {
      public:
        explicit function_call(const function_start * callee, std::vector<node *> args = {})
            : callee{callee}
            , arguments{std::move(args)} {}

        make_visitable;

        void flows_from(node * node) override { previous = node; }

        // Invariant: none of the following `node *` may be null
        node * previous{nullptr};
        node * next{nullptr};
        const function_start * callee;
        std::vector<node *> arguments;
    };

    // Handles both if expressions and if statements
    class branch final : public node {
      public:
        make_visitable;

        void flows_from(node * node) override { previous = node; }

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

        void flows_from(node * node) override { previous = node; }

        // Invariant: `previous` cannot be null
        node * previous;

        node * value;
    };

    // Handles joining control paths
    class phi final : public node {
      public:
        make_visitable;

        void flows_from(node * node) override {
            assert(node != nullptr);
            if (dynamic_cast<control_flow::function_end *>(node) == nullptr) {
                previous.push_back(node);
            }
        }

        // Invariant: none of the following `node *` may be null
        std::vector<node *> previous;
        node * next;
    };

#undef make_visitable

} // namespace control_flow

#endif
