#ifndef TOP_LVL_NODES_HPP
#define TOP_LVL_NODES_HPP

#include "base_nodes.hpp"
#include "node_utils.hpp"

// Classes relating to top level items

// TODO: Nested modules will make this extend from Top_Level
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

// TODO: make this class a member of Function and shorten the name
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

// Top Level classes
class Function final : public Top_Level {
  public:
    Function(Func_Header && head, Statement * body)
        : head_(std::move(head))
        , body_(body) {}

    non_copyable(Function);

    movable(Function);

    llvm::Value * codegen(context_module & context) override;

    bool type_check(context_module & context) override;

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

    llvm::Value * codegen(context_module & context) override;

    bool type_check(context_module & context) override;

  private:
    Typed_Var name_and_type;
    expr_ptr expr;
};

#endif
