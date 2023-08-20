#pragma once

#include "base_nodes.hpp"
#include "node_utils.hpp"
#include "type.hpp"

#include <map>

// Classes representing top level items

namespace ast {
    class top_level_sequence final : public top_level {
      public:
        explicit top_level_sequence(Location loc)
            : node{loc} {}

        non_copyable(top_level_sequence);

        movable(top_level_sequence);

        ~top_level_sequence() override = default;

        make_visitable;

        void append(top_lvl_ptr item) { items.emplace_back(std::move(item)); }

        void append(std::vector<top_lvl_ptr> && new_items) {
            for (auto && item : new_items) { items.emplace_back(std::move(item)); }
        }

        std::map<std::string, std::vector<std::string>> imports;

        std::vector<top_lvl_ptr> items;

        std::string filename;

      private:
        void update_export(bool val) final {
            for (auto & item : items) { item->should_export(val); }
        }
    };

    class func_decl final : public top_level {
      public:
        func_decl(std::string name, ast::function_type * func_type, stmt_ptr body, Location loc)
            : node{loc}
            , name{std::move(name)}
            , func_type{func_type}
            , body(std::move(body)) {}

        non_copyable(func_decl);

        movable(func_decl);

        ~func_decl() noexcept final = default;

        [[nodiscard]] size_t param_count() const { return params.size(); }

        make_visitable;

        std::string name;
        std::vector<typed_identifier> params;
        ast::function_type * func_type;
        stmt_ptr body;

      private:
        void update_export(bool /*val*/) final {}
    };

    class const_decl final : public top_level {
      public:
        const_decl(typed_identifier && name_and_type, expr_ptr expr, Location loc)
            : node{loc}
            , name_and_type(std::move(name_and_type))
            , expr{std::move(expr)} {}

        non_copyable(const_decl);

        movable(const_decl);

        ~const_decl() noexcept final = default;

        make_visitable;

        typed_identifier name_and_type;
        expr_ptr expr;

      private:
        void update_export(bool /*val*/) final {}
    };

    class struct_decl final : public top_level {
      public:
        explicit struct_decl(std::string name, ast::struct_type * type,
                             std::vector<typed_identifier> && fields, Location loc)
            : node{loc}
            , name(std::move(name))
            , fields{std::move(fields)}
            , type{type} {}

        non_copyable(struct_decl);

        movable(struct_decl);

        ~struct_decl() noexcept final = default;

        make_visitable;

        std::string name;
        std::vector<typed_identifier> fields;
        ast::struct_type * type;

      private:
        void update_export(bool /*val*/) final {}
    };
} // namespace ast
