#include "nodes.hpp"

#include "operations.hpp"
#include "top_lvl_nodes.hpp"

namespace ast {

    bool binary_expr::is_comparison() const noexcept { return operation::is_comparison(op); }

    bool binary_expr::is_shortcircuiting() const noexcept {
        return operation::is_shortcircuiting(op);
    }

    [[nodiscard]] bool binary_expr::is_arithmetic() const noexcept {
        return operation::is_arithmetic(op);
    }

    std::shared_ptr<ast::function_type> func_decl::func_type() const {

        std::vector<ast::type_ptr> param_types;
        for (const auto & param : params) { param_types.push_back(param.type()); }

        return ast::function_type::create(ret_type, std::move(param_types));
    }
} // namespace ast
