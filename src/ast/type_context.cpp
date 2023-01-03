#include "type_context.hpp"

#include "type.hpp"

#include <cassert>

namespace ast {
    type_context::type_context() {
        (void)create_type<ast::prim_type>(ast::prim_type::type::boolean);
        (void)create_type<ast::prim_type>(ast::prim_type::type::character);
        (void)create_type<ast::prim_type>(ast::prim_type::type::float32);
        (void)create_type<ast::prim_type>(ast::prim_type::type::int32);
        (void)create_type<ast::prim_type>(ast::prim_type::type::null);
        (void)create_type<ast::prim_type>(ast::prim_type::type::str);
        (void)create_type<ast::prim_type>(ast::prim_type::type::unit);
    }

    type_ptr type_context::find_prim_type(ast::prim_type::type type) {
        // We should always find a prim_type

        for (auto & type_ptr : types) {
            if (auto ptr = std::dynamic_pointer_cast<ast::prim_type>(type_ptr); ptr != nullptr) {
                if (ptr->inner() == type) { return ptr; }
            }
        }

        // TODO: We may want to lazily allocate prim_types
        assert(false);
    }

    type_ptr type_context::find_ptr_type(bool is_nullable, type_ptr pointed_to) {
        for (auto & type_ptr : types) {
            if (auto ptr = std::dynamic_pointer_cast<ast::ptr_type>(type_ptr); ptr != nullptr) {
                if (ptr->nullable() == is_nullable and ptr->pointed_to_type() == pointed_to) {
                    return ptr;
                }
            }
        }

        return is_nullable ? static_cast<std::shared_ptr<ast::ptr_type>>(
                   nullable_ptr_type::create(std::move(pointed_to)))
                           : nonnullable_ptr_type::create(std::move(pointed_to));
    }

    std::shared_ptr<function_type>
    type_context::find_function_type(type_ptr return_type, std::vector<type_ptr> && arg_types) {

        for (auto & type_ptr : types) {
            if (auto ptr = std::dynamic_pointer_cast<ast::function_type>(type_ptr);
                ptr != nullptr) {
                if (ptr->return_type() != return_type) { continue; }
                if (ptr->arg_count() != arg_types.size()) { continue; }

                for (auto i = 0UL; i < ptr->arg_count(); ++i) {
                    if (ptr->arg(i) != arg_types[i]) { continue; }
                }

                return ptr;
            }
        }

        return function_type::create(std::move(return_type), std::move(arg_types));
    }

    std::shared_ptr<ast::struct_type>
    type_context::find_struct_type(std::string && name, const std::string & module_name,
                                   std::vector<struct_type::field_type> && fields) {

        for (auto & type_ptr : types) {
            if (auto ptr = std::dynamic_pointer_cast<ast::struct_type>(type_ptr); ptr != nullptr) {
                if (ptr->containing_module_name() != module_name) { continue; }
                if (ptr->user_name() != name) { continue; }
                if (ptr->field_count() != fields.size()) { continue; }

                for (auto i = 0UL; i < ptr->field_count(); ++i) {
                    if (ptr->field(i) != fields[i]) { continue; }
                }

                return ptr;
            }
        }

        return struct_type::create(std::move(name), module_name, std::move(fields));
    }

    type_ptr type_context::lookup_user_type(const std::string & name,
                                            const std::string & module_name) {
        for (auto & type_ptr : types) {
            if (auto ptr = std::dynamic_pointer_cast<ast::user_type>(type_ptr); ptr != nullptr) {
                if (ptr->containing_module_name() == module_name and ptr->user_name() == name) {
                    return ptr;
                }
            }
        }

        return nullptr;
    }
} // namespace ast
