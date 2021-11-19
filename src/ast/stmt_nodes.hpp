#ifndef STMT_NODES_HPP
#define STMT_NODES_HPP

#include "base_nodes.hpp"
#include "node_utils.hpp"

// stmt classes

namespace ast {
    class if_stmt final : public stmt {
      public:
        if_stmt(expr * cond, stmt * on_true, stmt * on_false)
            : condition(cond)
            , true_branch(on_true)
            , else_branch(on_false) {}

        non_copyable(if_stmt);

        movable(if_stmt);

        make_visitable;

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

    class let_stmt final : public stmt {
      public:
        let_stmt(std::string && name, expr * value)
            : name_and_type(std::move(name), "auto")
            , value_(value) {}

        let_stmt(typed_identifier && typed_name, expr * value)
            : name_and_type(std::move(typed_name))
            , value_(value) {}

        non_copyable(let_stmt);

        movable(let_stmt);

        make_visitable;

        llvm::Value * codegen(context_module & context) override;

        bool type_check(context_module & context) override {
            auto * expr_type = value_->type_check(context);
            if (expr_type == nullptr) { return false; }

            auto * decl_type = context.find_type(name_and_type.type(), location());

            if (decl_type == nullptr or expr_type != decl_type) {
                context.printError("Let-binding for " + name_and_type.name()
                                       + " could not type check.",
                                   location());
                return false;
            }

            return context.bind_type(name_and_type.name(), expr_type);
        }

      private:
        typed_identifier name_and_type;
        expr_ptr value_;
    };

    class stmt_sequence final : public stmt {
      public:
        stmt_sequence() = default;
        stmt_sequence(stmt * stmt)
            : stmt_sequence() {
            append(stmt);
        }

        non_copyable(stmt_sequence);

        movable(stmt_sequence);

        ~stmt_sequence() override = default;

        void append(stmt * stmt) { stmts.emplace_back(stmt); }

        make_visitable;

        // The return value should not be used
        llvm::Value * codegen(context_module & context) override {
            for (const auto & entry : stmts) { entry->codegen(context); }
            return nullptr;
        }

        bool type_check(context_module & context) override {
            for (const auto & entry : stmts) {
                if (not entry->type_check(context)) { return false; }
            }
            return true;
        }

      private:
        std::vector<stmt_ptr> stmts{};
    };

    class func_call_stmt final : public stmt {
      public:
        explicit func_call_stmt(func_call_data && data)
            : data{std::move(data)} {}

        make_visitable;

        llvm::Value * codegen(context_module & context) override { return data.codegen(context); }

        bool type_check(context_module & context) override {
            // TODO: We may want to allow dropping return values
            return data.type_check(context, location()) == context.builder().getVoidTy();
        }

      private:
        func_call_data data;
    };

    class return_stmt final : public stmt {
      public:
        explicit return_stmt(expr * val = nullptr)
            : value(val) {}

        non_copyable(return_stmt);

        movable(return_stmt);

        make_visitable;

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
} // namespace ast

#endif
