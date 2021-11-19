#ifndef TOP_LVL_NODES_HPP
#define TOP_LVL_NODES_HPP

#include "base_nodes.hpp"
#include "node_utils.hpp"

// Classes relating to top level items

namespace ast {
    // TODO: Nested modules will make this extend from top_level
    class top_level_sequence final : public node {
      public:
        top_level_sequence() = default;
        top_level_sequence(top_level * first_item)
            : top_level_sequence{} {
            append(first_item);
        }

        non_copyable(top_level_sequence);

        movable(top_level_sequence);

        ~top_level_sequence() override = default;

        void append(top_level * item) { top_lvl_seq_.emplace_back(item); }

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

    // TODO: make this class a member of func_decl and shorten the name
    class func_header final {
      public:
        func_header(std::string && name, std::vector<typed_identifier> && parameters)
            : name_(std::move(name))
            , params(std::move(parameters)) {}

        void set_ret_type(std::string && type) { ret_type = std::move(type); }

        llvm::FunctionType * full_type(context_module & context);

        [[nodiscard]] const typed_identifier & arg(unsigned index) const {
            return params.at(index);
        }
        [[nodiscard]] const std::string & name() const { return name_; }

        void set_location(const Location & loc_new) { loc = loc_new; }

        [[nodiscard]] const auto & location() const noexcept { return loc; }

        void add_parameters(context_module &, llvm::Function &) const;

      private:
        std::vector<llvm::Type *> param_types(context_module & context);

        std::string name_;
        std::vector<typed_identifier> params;
        std::string ret_type{};
        Location loc{};
    };

    // Top Level classes
    class func_decl final : public top_level {
      public:
        func_decl(func_header && head, stmt * body)
            : head_(std::move(head))
            , body_(body) {}

        non_copyable(func_decl);

        movable(func_decl);

        llvm::Value * codegen(context_module & context) override;

        bool type_check(context_module & context) override;

      private:
        func_header head_;
        stmt_ptr body_;
    };

    class const_decl final : public top_level {
      public:
        const_decl(typed_identifier && name_and_type, expr * expr)
            : name_and_type(std::move(name_and_type))
            , expr{expr} {}

        non_copyable(const_decl);

        movable(const_decl);

        llvm::Value * codegen(context_module & context) override;

        bool type_check(context_module & context) override;

      private:
        typed_identifier name_and_type;
        expr_ptr expr;
    };
} // namespace ast

#endif
