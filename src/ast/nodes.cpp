#include "nodes.hpp"

#include "parser.hpp"

namespace ast {

    bool binary_expr::is_comparison() const noexcept {
        switch (tok) {
        case T_GE:
        case T_GT:
        case T_LT:
        case T_LE:
        case T_EQ:
        case T_NE:
            return true;
        default:
            return false;
        }
    }

    bool binary_expr::is_shortcircuiting() const noexcept { return tok == T_OR or tok == T_AND; }

} // namespace ast
