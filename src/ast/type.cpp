#include "type.hpp"

#include <algorithm> // find_if
#include <iostream>  // cout

#include <global_map.hpp>

namespace ast {
    // Type creation

    const type_ptr prim_type::int32 = type_ptr{new prim_type{ast::prim_type::type::int32}};
    const type_ptr prim_type::unit = type_ptr{new prim_type{ast::prim_type::type::unit}};
    const type_ptr prim_type::float32 = type_ptr{new prim_type{ast::prim_type::type::float32}};
    const type_ptr prim_type::boolean = type_ptr{new prim_type{ast::prim_type::type::boolean}};
    const type_ptr prim_type::str = type_ptr{new prim_type{ast::prim_type::type::str}};
    const type_ptr prim_type::character = type_ptr{new prim_type{ast::prim_type::type::character}};
    const type_ptr prim_type::null = type_ptr{new prim_type{ast::prim_type::type::null}};

    std::shared_ptr<nullable_ptr_type> nullable_ptr_type::create(type_ptr pointed_to_type) {
        return std::shared_ptr<nullable_ptr_type>{
            new nullable_ptr_type{std::move(pointed_to_type)}};
    }

    std::shared_ptr<nonnullable_ptr_type> nonnullable_ptr_type::create(type_ptr pointed_to_type) {
        return std::shared_ptr<nonnullable_ptr_type>{
            new nonnullable_ptr_type{std::move(pointed_to_type)}};
    }

    std::shared_ptr<struct_type> struct_type::create(std::string && name,
                                                     const std::string & module_name,
                                                     std::vector<field_type> && fields) {

        assert(not name.empty());
        // TODO: The "" module may be useful.
        assert(not module_name.empty());

        return std::shared_ptr<struct_type>{
            new struct_type{std::move(name), module_name, std::move(fields)}};
    }

    std::shared_ptr<function_type> function_type::create(ast::type_ptr ret_type,
                                                         std::vector<ast::type_ptr> && arg_types) {
        static std::vector<std::shared_ptr<function_type>> made_types;

        if (auto iter = std::find_if(made_types.begin(), made_types.end(),
                                     [&ret_type, &arg_types](const auto & exisiting_type) -> bool {
                                         return exisiting_type->ret_type == ret_type
                                            and exisiting_type->arg_types == arg_types;
                                     });
            iter != made_types.end()) {
            return *iter;
        }

        auto new_func_type = std::shared_ptr<function_type>{
            new function_type{std::move(ret_type), std::move(arg_types)}};
        made_types.push_back(new_func_type);

        return new_func_type;
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
        case type::null:
            lhs << "null";
            break;
        }
    }

    void struct_type::print(std::ostream & lhs) const {
        lhs << user_name() << '{';
        for (const auto & [name, type] : fields) { lhs << name << " : " << type << ", "; }
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
