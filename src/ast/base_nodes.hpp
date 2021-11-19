#ifndef BASE_NODES_HPP
#define BASE_NODES_HPP

#include "context_module.hpp"
#include "location.hpp"
#include "nodes_forward.hpp"
#include "utils/move_copy.hpp"
#include "visitor/visitor_base.hpp"
#include <llvm/IR/Value.h>

#include <memory> // unique_ptr

// This file should contain only node and any *abstract* child of it.

// Basic node
namespace ast {
    class node {
      public:
        node() = default;

        non_copyable(node);

        movable(node);

        virtual ~node() = default;

        virtual void accept(visitor::visitor_base &) = 0;

#define make_visitable \
    void accept(visitor::visitor_base & visitor) override { visitor.visit(*this); }

        virtual llvm::Value * codegen(context_module & context) = 0;

        void set_location(const Location & loc_new) { loc = loc_new; }

        [[nodiscard]] const auto & location() const noexcept { return loc; }

      private:
        Location loc{};
    };

    // Base classes
    class expr : public virtual node {
      public:
        virtual llvm::Constant * compile_time_codegen(context_module &) = 0;

        virtual llvm::Type * type_check(context_module &) = 0;
    };
    class stmt : public virtual node {
      public:
        [[nodiscard]] virtual bool type_check(context_module &) = 0;
    };
    class top_level : public virtual node {
      public:
        [[nodiscard]] virtual bool type_check(context_module &) = 0;
    };

    // Utility types aliases
    using expr_ptr = std::unique_ptr<expr>;
    using stmt_ptr = std::unique_ptr<stmt>;
    using top_lvl_ptr = std::unique_ptr<top_level>;
} // namespace ast

#endif
