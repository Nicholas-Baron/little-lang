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

        if_stmt(expr_ptr cond, stmt_ptr on_true, stmt_ptr on_false)
            : condition(std::move(cond))
            , true_branch(std::move(on_true))
            , else_branch(std::move(on_false)) {}

        non_copyable(if_stmt);

        movable(if_stmt);

        make_visitable;

        expr_ptr condition;
        stmt_ptr true_branch;
        stmt_ptr else_branch;
    };

    class let_stmt final : public stmt {
      public:
        let_stmt(std::string && name, expr * value)
            : name_and_type(std::move(name), "auto")
            , value(value) {}

        let_stmt(typed_identifier && typed_name, expr * value)
            : name_and_type(std::move(typed_name))
            , value(value) {}

        non_copyable(let_stmt);

        movable(let_stmt);

        make_visitable;

        typed_identifier name_and_type;
        expr_ptr value;
    };

    class stmt_sequence final : public stmt {
      public:
        stmt_sequence() = default;
        explicit stmt_sequence(stmt * stmt)
            : stmt_sequence() {
            append(stmt);
        }

        non_copyable(stmt_sequence);

        movable(stmt_sequence);

        ~stmt_sequence() override = default;

        void append(stmt * stmt) { stmts.emplace_back(stmt); }
        void append(stmt_ptr stmt) { stmts.push_back(std::move(stmt)); }

        make_visitable;

        std::vector<stmt_ptr> stmts{};
    };

    class func_call_stmt final : public stmt {
      public:
        explicit func_call_stmt(func_call_data && data)
            : data{std::move(data)} {}

        make_visitable;

        func_call_data data;
    };

    class return_stmt final : public stmt {
      public:
        explicit return_stmt(expr * val = nullptr)
            : value(val) {}

        non_copyable(return_stmt);

        movable(return_stmt);

        make_visitable;

        expr_ptr value;
    };
} // namespace ast

#endif
