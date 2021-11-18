#ifndef _NODE_HPP
#define _NODE_HPP

#include "base_nodes.hpp"
#include "context_module.hpp"
#include "location.hpp"
#include "nodes_forward.hpp"
#include "utils/move_copy.hpp"
#include <llvm/IR/Value.h>

// Classes that do not need Node

class Typed_Var final {
  public:
    Typed_Var(std::string && name, std::string && type)
        : type_{std::move(type)}
        , name_{std::move(name)} {}

    [[nodiscard]] const auto & name() const { return name_; }
    [[nodiscard]] const auto & type() const { return type_; }

    void set_location(const Location & loc_new) { loc = loc_new; }

    [[nodiscard]] const auto & location() const noexcept { return loc; }

  private:
    std::string type_;
    std::string name_;
    Location loc{};
};

class Func_Header final {
  public:
    Func_Header(std::string && name, std::vector<Typed_Var> && parameters)
        : name_(std::move(name))
        , params(std::move(parameters)) {}

    void set_ret_type(std::string && type) { ret_type = std::move(type); }

    llvm::FunctionType * full_type(context_module & context);

    [[nodiscard]] const Typed_Var & arg(unsigned index) const { return params.at(index); }
    [[nodiscard]] const std::string & name() const { return name_; }

    void set_location(const Location & loc_new) { loc = loc_new; }

    [[nodiscard]] const auto & location() const noexcept { return loc; }

    void add_parameters(context_module &, llvm::Function &) const;

  private:
    std::vector<llvm::Type *> param_types(context_module & context);

    std::string name_;
    std::vector<Typed_Var> params;
    std::string ret_type{};
    Location loc{};
};

// Stores the function call data.
// is facaded by func_call_expr and func_call_stmt
class func_call_data final {
  public:
    func_call_data(std::string && name, std::vector<expr_ptr> && args)
        : name_(std::move(name))
        , args_{std::move(args)} {}

    non_copyable(func_call_data);

    movable(func_call_data);

    llvm::Value * codegen(context_module & context);

    llvm::Type * type_check(context_module &, Location);

  private:
    std::string name_;
    std::vector<expr_ptr> args_{};
};

// Direct from Node classes
class Top_Level_Seq final : public Node {
  public:
    Top_Level_Seq() = default;
    Top_Level_Seq(Top_Level * first_item)
        : Top_Level_Seq{} {
        append(first_item);
    }

    non_copyable(Top_Level_Seq);

    movable(Top_Level_Seq);

    ~Top_Level_Seq() override = default;

    void append(Top_Level * item) { top_lvl_seq_.emplace_back(item); }

    // The return value should not be used
    llvm::Value * codegen(context_module & context) override {
        for (const auto & item : top_lvl_seq_) {
            assert(item != nullptr);
            item->codegen(context);
        }
        return nullptr;
    }

    [[nodiscard]] bool type_check(context_module & context) {
        for (auto & item : top_lvl_seq_) {
            assert(item != nullptr);
            if (not item->type_check(context)) { return false; }
        }
        return true;
    }

  private:
    std::vector<top_lvl_ptr> top_lvl_seq_;
};


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

// Top Level classes
class Function final : public Top_Level {
  public:
    Function(Func_Header && head, Statement * body)
        : head_(std::move(head))
        , body_(body) {}

    non_copyable(Function);

    movable(Function);

    llvm::Value * codegen(context_module & context) override;

    bool type_check(context_module & context) override {
        context.clean_type_bindings();
        auto * full_type = head_.full_type(context);
        for (auto i = 0U; i < full_type->getNumParams(); ++i) {
            if (not context.bind_type(head_.arg(i).name(), full_type->getParamType(i))) {
                return false;
            }
        }
        auto bind_return = context.bind_type("return", full_type->getReturnType());
        assert(bind_return);
        return body_->type_check(context);
    }

  private:
    Func_Header head_;
    stmt_ptr body_;
};

class Constant final : public Top_Level {
  public:
    Constant(Typed_Var && name_and_type, Expression * expr)
        : name_and_type(std::move(name_and_type))
        , expr{expr} {}

    non_copyable(Constant);

    movable(Constant);

    llvm::Value * codegen(context_module & /*context*/) override;

    bool type_check(context_module &) override;

  private:
    Typed_Var name_and_type;
    expr_ptr expr;
};

#endif
