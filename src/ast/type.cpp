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
} // namespace ast
