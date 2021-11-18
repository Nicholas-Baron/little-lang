#ifndef STMT_NODES_HPP
#define STMT_NODES_HPP

#include "base_nodes.hpp"
#include "nodes.hpp"

// Statement classes

class If_Statement final : public Statement {
  public:
    If_Statement(Expression * cond, Statement * on_true, Statement * on_false)
        : condition(cond)
        , true_branch(on_true)
        , else_branch(on_false) {}

    non_copyable(If_Statement);

    movable(If_Statement);

    llvm::Value * codegen(context_module & context) override;

    bool type_check(context_module & context) override {
        return condition->type_check(context) != nullptr and true_branch->type_check(context)
           and (else_branch == nullptr or else_branch->type_check(context));
    }

  private:
    expr_ptr condition;
    stmt_ptr true_branch;
    stmt_ptr else_branch;
};

class Let_Statement final : public Statement {
  public:
    Let_Statement(std::string && name, Expression * value)
        : name_and_type(std::move(name), "auto")
        , value_(value) {}

    Let_Statement(Typed_Var && typed_name, Expression * value)
        : name_and_type(std::move(typed_name))
        , value_(value) {}

    non_copyable(Let_Statement);

    movable(Let_Statement);

    llvm::Value * codegen(context_module & context) override;

    bool type_check(context_module & context) override {
        auto * expr_type = value_->type_check(context);
        if (expr_type == nullptr) { return false; }

        auto * decl_type = context.find_type(name_and_type.type(), location());

        if (decl_type == nullptr or expr_type != decl_type) {
            context.printError("Let-binding for " + name_and_type.name() + " could not type check.",
                               location());
            return false;
        }

        return context.bind_type(name_and_type.name(), expr_type);
    }

  private:
    Typed_Var name_and_type;
    expr_ptr value_;
};

class Statement_Seq final : public Statement {
  public:
    Statement_Seq() = default;
    Statement_Seq(Statement * stmt)
        : Statement_Seq() {
        append(stmt);
    }

    non_copyable(Statement_Seq);

    movable(Statement_Seq);

    ~Statement_Seq() override = default;

    void append(Statement * stmt) { statements.emplace_back(stmt); }

    // The return value should not be used
    llvm::Value * codegen(context_module & context) override {
        for (const auto & entry : statements) { entry->codegen(context); }
        return nullptr;
    }

    bool type_check(context_module & context) override {
        for (const auto & entry : statements) {
            if (not entry->type_check(context)) { return false; }
        }
        return true;
    }

  private:
    std::vector<stmt_ptr> statements{};
};

class func_call_stmt final : public Statement {
  public:
    explicit func_call_stmt(func_call_data && data)
        : data{std::move(data)} {}

    llvm::Value * codegen(context_module & context) override { return data.codegen(context); }

    bool type_check(context_module & context) override {
        // TODO: We may want to allow dropping return values
        return data.type_check(context, location()) == context.builder().getVoidTy();
    }

  private:
    func_call_data data;
};

class Return_Statement final : public Statement {
  public:
    explicit Return_Statement(Expression * val = nullptr)
        : value(val) {}

    non_copyable(Return_Statement);

    movable(Return_Statement);

    llvm::Value * codegen(context_module & context) override;

    bool type_check(context_module & context) override {
        auto * expr_type
            = value != nullptr ? value->type_check(context) : context.builder().getVoidTy();
        auto * decl_type = context.get_identifer_type("return");
        return expr_type == decl_type;
    }

  private:
    expr_ptr value;
};

#endif
