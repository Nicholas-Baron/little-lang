#ifndef EXPR_NODES_HPP
#define EXPR_NODES_HPP

#include "nodes.hpp"

// Expression node classes

class UserValue final : public Expression {
  public:
    UserValue(std::string && value)
        : val(std::move(value)) {}

    non_copyable(UserValue);

    movable(UserValue);

    llvm::Value * codegen(context_module & context) override;
    llvm::Constant * compile_time_codegen(context_module & context) override;

    llvm::Type * type_check(context_module & context) override;

  private:
    [[nodiscard]] llvm::ConstantInt * as_i32(context_module &) const;
    [[nodiscard]] llvm::ConstantInt * as_bool(context_module &) const;

    [[nodiscard]] bool is_bool() const;

    std::string val;
};

class UnaryExpression final : public Expression {
  public:
    UnaryExpression(int token, Expression * operand)
        : tok(token)
        , expr(operand) {}

    non_copyable(UnaryExpression);

    movable(UnaryExpression);

    llvm::Value * codegen(context_module & context) override;
    llvm::Constant * compile_time_codegen(context_module & context) override;
    llvm::Type * type_check(context_module & context) override { return expr->type_check(context); }

  private:
    int tok;
    expr_ptr expr;
};

class BinaryExpression final : public Expression {
  public:
    BinaryExpression(Expression * lhs, int op, Expression * rhs)
        : lhs_(lhs)
        , rhs_(rhs)
        , tok(op) {}

    non_copyable(BinaryExpression);

    movable(BinaryExpression);

    llvm::Value * codegen(context_module & context) override;

    llvm::Constant * compile_time_codegen(context_module & context) override;

    llvm::Type * type_check(context_module & context) override {
        auto * lhs_type = lhs_->type_check(context);
        auto * rhs_type = rhs_->type_check(context);

        if (lhs_type != rhs_type) {
            context.printError("Failed to type check binary expression", location());
            return nullptr;
        }

        if (is_comparison()) { return context.builder().getInt1Ty(); }
        return lhs_type;
    }

  private:
    [[nodiscard]] bool is_comparison() const noexcept;
    [[nodiscard]] bool is_shortcircuiting() const noexcept;

    expr_ptr lhs_, rhs_;
    int tok;
};

class func_call_expr final : public Expression {
  public:
    explicit func_call_expr(func_call_data && data)
        : data{std::move(data)} {}

    llvm::Value * codegen(context_module & context) override { return data.codegen(context); }

    llvm::ConstantExpr * compile_time_codegen(context_module & context) override {
        context.printError("Function call cannot be done in a compile time context", location());
        return nullptr;
    }

    llvm::Type * type_check(context_module & context) override {
        return data.type_check(context, location());
    }

  private:
    func_call_data data;
};

#endif
