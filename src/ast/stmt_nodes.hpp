#pragma once

#include "base_nodes.hpp"
#include "node_utils.hpp"

// stmt classes

namespace ast {
    class if_stmt final : public stmt {
      public:
        if_stmt(expr_ptr cond, stmt_ptr on_true, stmt_ptr on_false, Location loc)
            : node{loc}
            , condition(std::move(cond))
            , true_branch(std::move(on_true))
            , else_branch(std::move(on_false)) {}

        non_copyable(if_stmt);

        movable(if_stmt);

        ~if_stmt() noexcept final = default;

        make_visitable;

        expr_ptr condition;
        stmt_ptr true_branch;
        stmt_ptr else_branch;
    };

    class let_stmt final : public stmt {
      public:
        let_stmt(typed_identifier && typed_name, expr_ptr value, Location loc)
            : node{loc}
            , name_and_type(std::move(typed_name))
            , value(std::move(value)) {}

        non_copyable(let_stmt);

        movable(let_stmt);

        ~let_stmt() noexcept final = default;

        make_visitable;

        typed_identifier name_and_type;
        expr_ptr value;
    };

    class stmt_sequence final : public stmt {
      public:
        explicit stmt_sequence(Location loc)
            : node{loc} {}

        non_copyable(stmt_sequence);

        movable(stmt_sequence);

        ~stmt_sequence() override = default;

        void append(stmt_ptr stmt) { stmts.push_back(std::move(stmt)); }

        make_visitable;

        std::vector<stmt_ptr> stmts{};
    };

    class func_call_stmt final : public stmt {
      public:
        explicit func_call_stmt(func_call_data && data)
            : node{data.location()}
            , data{std::move(data)} {}

        make_visitable;

        func_call_data data;
    };

    class return_stmt final : public stmt {
      public:
        explicit return_stmt(Location loc, expr_ptr val = nullptr)
            : node{loc}
            , value(std::move(val)) {}

        non_copyable(return_stmt);

        movable(return_stmt);

        ~return_stmt() noexcept final = default;

        make_visitable;

        expr_ptr value;
    };
} // namespace ast
