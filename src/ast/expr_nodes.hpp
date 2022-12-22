#ifndef EXPR_NODES_HPP
#define EXPR_NODES_HPP

#include "base_nodes.hpp"
#include "location.hpp"
#include "node_utils.hpp"

#include <literal_type.hpp>
#include <operations.hpp>

// expr node classes

namespace ast {
    // TODO: Mark some AST nodes as constants
    class user_val final : public expr {
      public:
        user_val(std::string && value, literal_type type, Location loc = {})
            : val(std::move(value))
            , val_type{type} {
            set_location(loc);
        }

        non_copyable(user_val);

        movable(user_val);

        ~user_val() noexcept final = default;

        make_visitable;

        std::string val;
        literal_type val_type;
    };

    class unary_expr final : public expr {
      public:
        // NOLINTNEXTLINE
        unary_expr(operation::unary op, expr_ptr operand)
            : op(op)
            , expr(std::move(operand)) {}

        non_copyable(unary_expr);

        movable(unary_expr);

        ~unary_expr() noexcept final = default;

        make_visitable;

        operation::unary op;
        expr_ptr expr;
    };

    class binary_expr final : public expr {
      public:
        // NOLINTNEXTLINE
        binary_expr(expr_ptr lhs, operation::binary op, expr_ptr rhs)
            : lhs(std::move(lhs))
            , rhs(std::move(rhs))
            , op(op) {}

        non_copyable(binary_expr);

        movable(binary_expr);

        make_visitable;

        ~binary_expr() noexcept final = default;

        [[nodiscard]] bool is_arithmetic() const noexcept;
        [[nodiscard]] bool is_comparison() const noexcept;
        [[nodiscard]] bool is_shortcircuiting() const noexcept;

        expr_ptr lhs, rhs;
        operation::binary op;
    };

    class if_expr final : public expr {
      public:
        if_expr(expr_ptr condition, expr_ptr then_case, expr_ptr else_case)
            : condition{std::move(condition)}
            , then_case{std::move(then_case)}
            , else_case{std::move(else_case)} {}

        non_copyable(if_expr);

        movable(if_expr);

        ~if_expr() noexcept final = default;

        make_visitable;

        expr_ptr condition, then_case, else_case;
    };

    class func_call_expr final : public expr {
      public:
        explicit func_call_expr(func_call_data && data, Location loc = {})
            : data{std::move(data)} {
            set_location(loc);
        }

        make_visitable;

        func_call_data data;
    };

    class struct_init final : public expr {
      public:
        struct_init(std::string name,
                    std::vector<std::pair<std::string, ast::expr_ptr>> && initializers)
            : type_name{std::move(name)}
            , initializers{std::move(initializers)} {}

        make_visitable;

        std::string type_name;
        std::vector<std::pair<std::string, ast::expr_ptr>> initializers;
    };
} // namespace ast

#endif
