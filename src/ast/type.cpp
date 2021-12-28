#include "type.hpp"

#include <iostream>

namespace ast {
    void nullable_ptr_type::print(std::ostream & lhs) const {
        lhs << "nullable ptr to " << *pointed_to;
    }

    void nonnullable_ptr_type::print(std::ostream & lhs) const {
        lhs << "nonnullable ptr to " << *pointed_to;
    }

    void prim_type::print(std::ostream & lhs) const {
        switch (prim) {
        case type::boolean:
            lhs << "bool";
            break;
        case type::int32:
            lhs << "int32";
            break;
        case type::float32:
            lhs << "float32";
            break;
        case type::character:
            lhs << "character";
            break;
        case type::str:
            lhs << "string";
            break;
        case type::unit:
            lhs << "unit";
            break;
        }
    }

    void user_type::print(std::ostream & lhs) const { lhs << name; }

    void function_type::print(std::ostream & lhs) const {
        lhs << '(';
        bool first = true;
        for (const auto & arg : arg_types) {
            if (first) {
                first = false;
            } else {
                lhs << ", ";
            }
            lhs << *arg;
        }
        lhs << ") -> " << *return_type;
    }

    [[nodiscard]] bool function_type::equals(const ast::type & rhs) const {
        const auto * rhs_cast = dynamic_cast<const function_type *>(&rhs);
        if (rhs_cast == nullptr) { return false; }
        if (rhs_cast->arg_types.size() != arg_types.size()) { return false; }
        for (auto i = 0U; i < arg_types.size(); ++i) {
            auto * lhs_arg = arg_types[i].get();
            auto * rhs_arg = rhs_cast->arg_types[i].get();
            if (lhs_arg == rhs_arg) { continue; }
            if (*lhs_arg != *rhs_arg) { return false; }
        }
        return *rhs_cast->return_type == *return_type;
    }
} // namespace ast
