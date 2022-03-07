#ifndef TOP_LVL_NODES_HPP
#define TOP_LVL_NODES_HPP

#include "base_nodes.hpp"
#include "node_utils.hpp"

#include <map>

// Classes representing top level items

namespace ast {
    class top_level_sequence final : public top_level {
      public:
        top_level_sequence() = default;

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
        class header final {
          public:
            header(std::string && name, std::vector<typed_identifier> && parameters)
                : name_(std::move(name))
                , params(std::move(parameters))
                , ret_type_{std::make_shared<ast::prim_type>(ast::prim_type::type::unit)} {}

            void set_ret_type(ast::type_ptr && type) { ret_type_ = std::move(type); }

            [[nodiscard]] ast::type_ptr ret_type() const { return ret_type_; }

            [[nodiscard]] const typed_identifier & arg(unsigned index) const {
                return params.at(index);
            }

            [[nodiscard]] size_t param_count() const { return params.size(); }

            [[nodiscard]] const std::string & name() const & { return name_; }

            void set_location(const Location & loc_new) { loc = loc_new; }

            [[nodiscard]] const auto & location() const noexcept { return loc; }

          private:
            std::string name_;
            std::vector<typed_identifier> params;
            ast::type_ptr ret_type_;
            Location loc{};
        };

        func_decl(header && head, stmt_ptr body)
            : head(std::move(head))
            , body(std::move(body)) {}

        non_copyable(func_decl);

        movable(func_decl);

        ~func_decl() noexcept final = default;

        make_visitable;

        header head;
        stmt_ptr body;

      private:
        void update_export(bool /*val*/) final {}
    };

    class const_decl final : public top_level {
      public:
        const_decl(typed_identifier && name_and_type, expr_ptr expr)
            : name_and_type(std::move(name_and_type))
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
} // namespace ast

#endif
