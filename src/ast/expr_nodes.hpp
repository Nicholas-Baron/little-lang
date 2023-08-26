#pragma once

#include "base_nodes.hpp"
#include "location.hpp"
#include "node_utils.hpp"

#include <literal_type.hpp>
#include <operations.hpp>

// expr node classes

namespace ast {
    class user_val final : public expr {
      public:
        user_val(std::string && value, literal_type type, Location loc)
            : node{loc}
            , val(std::move(value))
            , val_type{type} {}

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
        unary_expr(operation::unary op, expr_ptr operand, Location loc)
            : node{loc}
            , op(op)
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
        binary_expr(expr_ptr lhs, operation::binary op, expr_ptr rhs, Location loc)
            : node{loc}
            , lhs(std::move(lhs))
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
        if_expr(Location loc, expr_ptr condition, expr_ptr then_case, expr_ptr else_case)
            : node{loc}
            , condition{std::move(condition)}
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
        explicit func_call_expr(func_call_data && data, Location loc)
            : node{loc}
            , data{std::move(data)} {}

        make_visitable;

        func_call_data data;
    };

    class struct_init final : public expr {
      public:
        struct_init(std::string name,
                    std::vector<std::pair<std::string, ast::expr_ptr>> && initializers,
                    Location loc)
            : node{loc}
            , type_name{std::move(name)}
            , initializers{std::move(initializers)} {}

        make_visitable;

        std::string type_name;
        std::vector<std::pair<std::string, ast::expr_ptr>> initializers;
    };

    class cast_expr final : public expr {
      public:
        cast_expr(ast::expr_ptr source, ast::type_ptr destination, Location loc)
            : node{loc}
            , operand{std::move(source)}
            , destination{destination} {}

        make_visitable;

        ast::expr_ptr operand;
        ast::type_ptr destination;
    };
} // namespace ast
