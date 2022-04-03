#include "type.hpp"

#include <iostream>

namespace ast {
    // Type creation

    const type_ptr prim_type::int32 = type_ptr{new prim_type{ast::prim_type::type::int32}};
    const type_ptr prim_type::unit = type_ptr{new prim_type{ast::prim_type::type::unit}};
    const type_ptr prim_type::float32 = type_ptr{new prim_type{ast::prim_type::type::float32}};
    const type_ptr prim_type::boolean = type_ptr{new prim_type{ast::prim_type::type::boolean}};
    const type_ptr prim_type::str = type_ptr{new prim_type{ast::prim_type::type::str}};
    const type_ptr prim_type::character = type_ptr{new prim_type{ast::prim_type::type::character}};

    std::shared_ptr<nullable_ptr_type> nullable_ptr_type::create(type_ptr pointed_to_type) {
        static std::map<type_ptr, std::shared_ptr<nullable_ptr_type>> made_types;

        if (auto iter = made_types.find(pointed_to_type); iter != made_types.end()) {
            return iter->second;
        }

        auto new_ptr_type
            = std::shared_ptr<nullable_ptr_type>{new nullable_ptr_type{pointed_to_type}};
        made_types.emplace(std::move(pointed_to_type), new_ptr_type);

        return new_ptr_type;
    }

    std::shared_ptr<nonnullable_ptr_type> nonnullable_ptr_type::create(type_ptr pointed_to_type) {
        static std::map<type_ptr, std::shared_ptr<nonnullable_ptr_type>> made_types;

        if (auto iter = made_types.find(pointed_to_type); iter != made_types.end()) {
            return iter->second;
        }

        auto new_ptr_type
            = std::shared_ptr<nonnullable_ptr_type>{new nonnullable_ptr_type{pointed_to_type}};
        made_types.emplace(std::move(pointed_to_type), new_ptr_type);

        return new_ptr_type;
    }

    std::shared_ptr<user_type> user_type::create(std::string && name) {
        static std::map<std::string, std::shared_ptr<user_type>> made_types;

        if (auto iter = made_types.find(name); iter != made_types.end()) { return iter->second; }

        auto new_user_type = std::shared_ptr<user_type>{new user_type{name}};
        made_types.emplace(std::move(name), new_user_type);

        return new_user_type;
    }

    // Printing types

    void nullable_ptr_type::print(std::ostream & lhs) const {
        lhs << "nullable ptr to " << *pointed_to_type();
    }

    void nonnullable_ptr_type::print(std::ostream & lhs) const {
        lhs << "nonnullable ptr to " << *pointed_to_type();
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
        lhs << ") -> " << *return_type();
    }

} // namespace ast
