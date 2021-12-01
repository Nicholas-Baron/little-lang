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
        explicit top_level_sequence(top_level * first_item)
            : top_level_sequence{} {
            append(first_item);
        }

        non_copyable(top_level_sequence);

        movable(top_level_sequence);

        ~top_level_sequence() override = default;

        make_visitable;

        void append(top_level * item) { items.emplace_back(item); }

        std::vector<top_lvl_ptr> items;
    };

    // Top Level classes
    class func_decl final : public top_level {
      public:
        class header final {
          public:
            header(std::string && name, std::vector<typed_identifier> && parameters)
                : name_(std::move(name))
                , params(std::move(parameters)) {}

            void set_ret_type(std::string && type) { ret_type_ = std::move(type); }

            [[nodiscard]] const auto & ret_type() const { return ret_type_; }

            [[nodiscard]] const typed_identifier & arg(unsigned index) const {
                return params.at(index);
            }

            [[nodiscard]] size_t param_count() const { return params.size(); }

            [[nodiscard]] const std::string & name() const { return name_; }

            void set_location(const Location & loc_new) { loc = loc_new; }

            [[nodiscard]] const auto & location() const noexcept { return loc; }

          private:
            std::string name_;
            std::vector<typed_identifier> params;
            std::string ret_type_{};
            Location loc{};
        };

        func_decl(header && head, stmt * body)
            : head(std::move(head))
            , body(body) {}

        non_copyable(func_decl);

        movable(func_decl);

        make_visitable;

        header head;
        stmt_ptr body;
    };

    class const_decl final : public top_level {
      public:
        const_decl(typed_identifier && name_and_type, expr * expr)
            : name_and_type(std::move(name_and_type))
            , expr{expr} {}

        non_copyable(const_decl);

        movable(const_decl);

        make_visitable;

        typed_identifier name_and_type;
        expr_ptr expr;
    };
} // namespace ast

#endif
