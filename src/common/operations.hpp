#pragma once

namespace operation {
    enum class binary {
        add,
        sub,
        mult,
        div,
        mod,
        gt,
        ge,
        lt,
        le,
        eq,
        ne,
        bool_and,
        bool_or,
        member_access,
    };

    [[nodiscard]] constexpr bool is_comparison(binary operat) noexcept {
        switch (operat) {
        case binary::ge:
        case binary::gt:
        case binary::lt:
        case binary::le:
        case binary::eq:
        case binary::ne:
            return true;
        default:
            return false;
        }
    }

    [[nodiscard]] constexpr bool is_shortcircuiting(binary operat) noexcept {
        return operat == binary::bool_or or operat == binary::bool_and;
    }

    [[nodiscard]] constexpr bool is_arithmetic(binary operat) noexcept {
        switch (operat) {
        case binary::add:
        case binary::sub:
        case binary::mult:
        case binary::div:
        case binary::mod:
            return true;
        default:
            return false;
        }
    }

    enum class unary { bool_not, negate, deref, addrof };
} // namespace operation
