#pragma once

#include "ast/type.hpp"
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

        [[nodiscard]] virtual bool allows_widening() const { return false; }

        // TODO: Put in constructor
        Location source_location;

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

        node * next{nullptr};
        ast::function_type * type;
        std::vector<std::string> parameter_names;
        std::string name;
        size_t arg_count;
        bool exported;
    };

    class binary_operation final : public node {
      public:
        binary_operation(node * lhs, operation::binary bin_op, node * rhs)
            : lhs{lhs}
            , rhs{rhs}
            , op{bin_op} {}

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

        [[nodiscard]] bool allows_widening() const override {
            return lhs->allows_widening() and rhs->allows_widening();
        }

        node * previous{nullptr};
        node * next{nullptr};
        node * lhs;
        node * rhs;
        ast::type_ptr result_type{nullptr};
        operation::binary op;
    };

    class struct_init final : public node {
      public:
        make_visitable;

        void flows_from(node * node) override { previous = node; }

        void flows_to(node * node) override { next = node; }

        node * previous{nullptr};
        node * next{nullptr};
        std::map<std::string, node *> fields;
        ast::struct_type * result_type{nullptr};
    };

    class member_access final : public node {
      public:
        member_access(node * lhs, std::string member_name)
            : lhs{lhs}
            , member_name{std::move(member_name)} {}

        make_visitable;

        void flows_from(node * node) override { previous = node; }

        void flows_to(node * node) override { next = node; }

        node * previous{nullptr};
        node * next{nullptr};
        node * lhs;
        std::string member_name;
        // The type_checker needs to set this
        std::optional<unsigned> member_index;
        ast::type_ptr result_type{nullptr};
    };

    class unary_operation final : public node {
      public:
        unary_operation(node * operand, operation::unary un_op)
            : operand{operand}
            , op{un_op} {}

        make_visitable;

        void flows_from(node * node) override { previous = node; }

        void flows_to(node * node) override { next = node; }

        [[nodiscard]] bool allows_widening() const override { return operand->allows_widening(); }

        node * previous{nullptr};
        node * next{nullptr};
        node * operand;
        ast::type_ptr result_type{nullptr};
        operation::unary op;
    };

    // TODO: This will not be the case when mutability is added.
    class constant final : public node {
      public:
        using value_variant_t = std::variant<std::monostate, long, double, char, bool, std::string>;

        constant(value_variant_t value, literal_type val_type)
            : value{std::move(value)}
            , val_type{val_type} {}

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

        [[nodiscard]] bool allows_widening() const override {
            return val_type == literal_type::integer or val_type == literal_type::floating;
        }

        node * previous{nullptr};
        node * next{nullptr};
        value_variant_t value;
        literal_type val_type;
        ast::type_ptr type{nullptr};
    };

    class intrinsic_call final : public node {
      public:
        explicit intrinsic_call(std::string name, std::vector<node *> args = {})
            : name{std::move(name)}
            , arguments{std::move(args)} {}

        make_visitable;

        void flows_from(node * node) override { previous = node; }

        void flows_to(node * node) override { next = node; }

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

        node * previous{nullptr};
        node * next{nullptr};
        const function_start * callee;
        std::vector<node *> arguments;
    };

    // Handles both if expressions and if statements
    class branch final : public node {
      public:
        explicit branch(node * condition)
            : condition_value{condition} {}

        make_visitable;

        void flows_from(node * node) override { previous = node; }

        void flows_to(node * node) override {
            if (node == true_case or node == false_case) { return; }

            if (true_case != nullptr and false_case != nullptr) {
                assert(false and "branch that is filled is being set again");
            }

            if (true_case == nullptr and false_case == nullptr) {
                assert(false and "branch has a null true and false cases");
            }

            (true_case == nullptr ? true_case : false_case) = node;
            assert(true_case != false_case);
        }

        node * previous{nullptr};
        node * condition_value;
        node * true_case{nullptr};
        node * false_case{nullptr};
    };

    // Handles return statements and "fall off the end"
    class function_end final : public node {
      public:
        make_visitable;

        void flows_from(node * node) override { previous = node; }

        void flows_to(node * /*node*/) override {
            assert(false and "function_end cannot have a next");
        }

        node * previous{nullptr};
        node * value{nullptr};
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

        std::vector<node *> previous;
        node * next{nullptr};
        ast::type_ptr type{nullptr};
    };

    class cast final : public node {
      public:
        cast(node * value, ast::type_ptr type)
            : value{value}
            , type{type} {}

        make_visitable;

        void flows_from(node * node) override { previous = node; }

        void flows_to(node * node) override { next = node; }

        node * previous{nullptr};
        node * next{nullptr};
        node * value;
        ast::type_ptr type;
    };

#undef make_visitable

} // namespace control_flow
