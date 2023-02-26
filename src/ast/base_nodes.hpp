#pragma once

#include "location.hpp"
#include "nodes_forward.hpp"
#include "type.hpp"
#include "visitor_base.hpp"

#include <memory> // unique_ptr

#include <move_copy.hpp>

// This file should contain only node and any *abstract* child of it.

// Basic node
namespace ast {
#undef make_visitable

    class node {
      public:
        node() = default;

        non_copyable(node);

        movable(node);

        virtual ~node() = default;

        virtual void accept(visitor_base &) = 0;

#define make_visitable \
    void accept(visitor_base & visitor) override { visitor.visit(*this); }

        // TODO: Just public location
        void set_location(const Location & loc_new) { loc = loc_new; }

        [[nodiscard]] const auto & location() const noexcept { return loc; }

      private:
        Location loc{};
    };

    // Base classes
    class expr : public virtual node {
      public:
        ast::type_ptr type{nullptr};
    };

    class stmt : public virtual node {};

    class top_level : public virtual node {

      public:
        [[nodiscard]] bool exported() const { return export_flag; }

        void should_export(bool val) {
            if (val != export_flag) {
                export_flag = val;
                update_export(val);
            }
        }

      protected:
        virtual void update_export(bool val) = 0;

      private:
        bool export_flag{false};
    };

    // Utility types aliases
    using expr_ptr = std::unique_ptr<expr>;
    using stmt_ptr = std::unique_ptr<stmt>;
    using top_lvl_ptr = std::unique_ptr<top_level>;
} // namespace ast
