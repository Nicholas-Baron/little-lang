#ifndef EXPR_NODES_HPP
#define EXPR_NODES_HPP

#include "base_nodes.hpp"
#include "location.hpp"
#include "node_utils.hpp"

// expr node classes

namespace ast {
    class user_val final : public expr {
      public:
        enum class value_type { identifier, integer, floating, character, boolean, string };

        user_val(std::string && value, value_type type)
            : val(std::move(value))
            , type{type} {}

        non_copyable(user_val);

        movable(user_val);

        make_visitable;

        std::string val;
        value_type type;
    };

    class unary_expr final : public expr {
      public:
        unary_expr(int token, expr * operand)
            : tok(token)
            , expr(operand) {}

        non_copyable(unary_expr);

        movable(unary_expr);

        make_visitable;

        int tok;
        expr_ptr expr;
    };

    class binary_expr final : public expr {
      public:
        binary_expr(expr * lhs, int op, expr * rhs)
            : lhs(lhs)
            , rhs(rhs)
            , tok(op) {}

        non_copyable(binary_expr);

        movable(binary_expr);

        make_visitable;

        [[nodiscard]] bool is_comparison() const noexcept;
        [[nodiscard]] bool is_shortcircuiting() const noexcept;

        expr_ptr lhs, rhs;
        int tok;
    };

    class func_call_expr final : public expr {
      public:
        explicit func_call_expr(func_call_data && data)
            : data{std::move(data)} {}

        make_visitable;

        func_call_data data;
    };
} // namespace ast

#endif
