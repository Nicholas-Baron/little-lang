#ifndef NODE_HPP
#define NODE_HPP

#include "common/operations.hpp"
#include "literal_type.hpp"
#include "visitor.hpp"

#include <cassert>
#include <memory>
#ifdef DEBUG
#include <iostream>
#endif
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

        // Makes `node` the next of `this`.
        virtual void flows_to(node * node) = 0;

      protected:
        node() = default;
    };

#define make_visitable \
    virtual void accept(visitor & visitor) override { visitor.visit(*this); }

    class function_start final : public node {
      public:
        function_start(std::string name, size_t arg_count, bool exported, ast::function_type * type)
            : type{type}
            , name{std::move(name)}
            , arg_count{arg_count}
            , exported{exported} {}

        make_visitable;

        void flows_from(node * /*node*/) override {
            assert(false and "There is no previous to a function_start");
        }

        void flows_to(node * node) override { next = node; }

        // Invariant: cannot be null
        node * next{nullptr};
        ast::function_type * type;
        std::vector<std::string> parameter_names;
        std::string name;
        size_t arg_count;
        bool exported;
    };

    class binary_operation final : public node {
      public:
        make_visitable;

        void flows_from(node * node) override {
#ifdef DEBUG
            std::cout << std::hex << '[' << reinterpret_cast<std::uintptr_t>(this) << "] Replacing "
                      << reinterpret_cast<std::uintptr_t>(previous) << " with "
                      << reinterpret_cast<std::uintptr_t>(node) << std::endl;
#endif
            previous = node;
        }

        void flows_to(node * node) override { next = node; }

        // Invariant: none of the following `node *` may be null
        node * previous;
        node * next;
        node * lhs;
        node * rhs;
        operation::binary op;
    };

    class unary_operation final : public node {
      public:
        make_visitable;

        void flows_from(node * node) override { previous = node; }

        void flows_to(node * node) override { next = node; }

        // Invariant: none of the following `node *` may be null
        node * previous;
        node * next;
        node * operand;
        operation::unary op;
    };

    // TODO: This will not be the case when mutability is added.
    class constant final : public node {
      public:
        make_visitable;

        void flows_from(node * node) override {
#ifdef DEBUG
            std::cout << std::hex << '[' << reinterpret_cast<std::uintptr_t>(this) << "] Replacing "
                      << reinterpret_cast<std::uintptr_t>(previous) << " with "
                      << reinterpret_cast<std::uintptr_t>(node) << std::endl;
#endif
            previous = node;
        }

        void flows_to(node * node) override { next = node; }

        std::variant<std::monostate, long, double, char, bool, std::string> value;
        literal_type val_type;

        // Invariant: none of the following `node *` may be null
        node * previous{nullptr};
        node * next{nullptr};
    };

    class intrinsic_call final : public node {
      public:
        explicit intrinsic_call(std::string name, std::vector<node *> args = {})
            : name{std::move(name)}
            , arguments{std::move(args)} {}

        make_visitable;

        void flows_from(node * node) override { previous = node; }

        void flows_to(node * node) override { next = node; }

        // Invariant: none of the following `node *` may be null
        node * previous{nullptr};
        node * next{nullptr};
        std::string name;
        ast::function_type * type{nullptr};
        std::vector<node *> arguments;
    };

    class function_call final : public node {
      public:
        explicit function_call(const function_start * callee, std::vector<node *> args = {})
            : callee{callee}
            , arguments{std::move(args)} {}

        make_visitable;

        void flows_from(node * node) override { previous = node; }

        void flows_to(node * node) override { next = node; }

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

        void flows_to(node * node) override {
            if (true_case != nullptr and false_case != nullptr) { return; }

            if (true_case == nullptr and false_case == nullptr) {
                assert(false and "branch has a null true and false cases");
            }

            (true_case == nullptr ? true_case : false_case) = node;
        }

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

        void flows_to(node * /*node*/) override {
            assert(false and "function_end cannot have a next");
        }

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

        void flows_to(node * node) override { next = node; }

        // Invariant: none of the following `node *` may be null
        std::vector<node *> previous;
        node * next;
    };

#undef make_visitable

} // namespace control_flow

#endif
