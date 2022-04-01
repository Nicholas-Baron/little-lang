#include "type.hpp"

#include <iostream>

namespace ast {
    void nullable_ptr_type::print(std::ostream & lhs) const {
        lhs << "nullable ptr to " << *pointed_to;
    }

    void nonnullable_ptr_type::print(std::ostream & lhs) const {
        lhs << "nonnullable ptr to " << *pointed_to;
    }

    const type_ptr prim_type::int32 = type_ptr{new prim_type{ast::prim_type::type::int32}};
    const type_ptr prim_type::unit = type_ptr{new prim_type{ast::prim_type::type::unit}};
    const type_ptr prim_type::float32 = type_ptr{new prim_type{ast::prim_type::type::float32}};
    const type_ptr prim_type::boolean = type_ptr{new prim_type{ast::prim_type::type::boolean}};
    const type_ptr prim_type::str = type_ptr{new prim_type{ast::prim_type::type::str}};
    const type_ptr prim_type::character = type_ptr{new prim_type{ast::prim_type::type::character}};

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

} // namespace ast
