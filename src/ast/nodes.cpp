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

} // namespace ast
