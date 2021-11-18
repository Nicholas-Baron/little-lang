#ifndef BASE_NODES_HPP
#define BASE_NODES_HPP

#include "context_module.hpp"
#include "location.hpp"
#include "nodes_forward.hpp"
#include "utils/move_copy.hpp"
#include <llvm/IR/Value.h>

#include <memory> // unique_ptr

// This file should contain only Node and any *abstract* child of it.

// Basic Node
namespace ast {
    class Node {
      public:
        Node() = default;

        non_copyable(Node);

        movable(Node);

        virtual ~Node() = default;

        virtual llvm::Value * codegen(context_module & context) = 0;

        void set_location(const Location & loc_new) { loc = loc_new; }

        [[nodiscard]] const auto & location() const noexcept { return loc; }

      private:
        Location loc{};
    };

    // Base classes
    class Expression : public virtual Node {
      public:
        virtual llvm::Constant * compile_time_codegen(context_module &) = 0;

        virtual llvm::Type * type_check(context_module &) = 0;
    };
    class Statement : public virtual Node {
      public:
        [[nodiscard]] virtual bool type_check(context_module &) = 0;
    };
    class Top_Level : public virtual Node {
      public:
        [[nodiscard]] virtual bool type_check(context_module &) = 0;
    };

    // Utility types aliases
    using expr_ptr = std::unique_ptr<Expression>;
    using stmt_ptr = std::unique_ptr<Statement>;
    using top_lvl_ptr = std::unique_ptr<Top_Level>;
} // namespace ast

#endif
