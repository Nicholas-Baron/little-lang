#include "type.hpp"

#include <algorithm> // find_if
#include <cassert>
#include <iostream> // cout
#include <memory>
#include <utility>

namespace ast {
    // Type creation

    std::unique_ptr<nullable_ptr_type> nullable_ptr_type::create(type_ptr pointed_to_type) {
        return std::unique_ptr<nullable_ptr_type>{new nullable_ptr_type{pointed_to_type}};
    }

    std::unique_ptr<nonnullable_ptr_type> nonnullable_ptr_type::create(type_ptr pointed_to_type) {
        return std::unique_ptr<nonnullable_ptr_type>{new nonnullable_ptr_type{pointed_to_type}};
    }

    std::unique_ptr<prim_type> prim_type::create(prim_type::type type) {
        if (type == prim_type::type::int_prim) {
            assert(false and "tried to initialize an integer type without a bit_width");
        }
        return std::unique_ptr<prim_type>{new prim_type{type}};
    }

    std::unique_ptr<int_type> int_type::create(unsigned size) {
        return std::unique_ptr<int_type>{new int_type{size}};
    }

    std::unique_ptr<struct_type> struct_type::create(std::string && name,
                                                     const std::string & module_name,
                                                     std::vector<field_type> && fields) {

        assert(not name.empty());
        // TODO: The "" module may be useful.
        assert(not module_name.empty());

        return std::unique_ptr<struct_type>{
            new struct_type{std::move(name), module_name, std::move(fields)}
        };
    }

    std::unique_ptr<function_type> function_type::create(ast::type_ptr ret_type,
                                                         std::vector<ast::type_ptr> && arg_types) {
        return std::unique_ptr<function_type>{
            new function_type{ret_type, std::move(arg_types)}
        };
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
        case type::int_prim:
            lhs << "int";
            break;
        case type::boolean:
            lhs << "bool";
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
        case type::null:
            lhs << "null";
            break;
        }
    }

    void int_type::print(std::ostream & lhs) const { lhs << "int" << size; }

    void struct_type::print(std::ostream & lhs) const {
        lhs << user_name() << '{';
        for (const auto & [name, type] : fields) { lhs << name << " : " << *type << ", "; }
        lhs << '}';
    }

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
