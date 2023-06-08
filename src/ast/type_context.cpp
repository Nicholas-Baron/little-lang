#include "type_context.hpp"

#include "type.hpp"

namespace ast {

    template<typename type_t, typename... args_t>
    type_t * type_context::emplace_type(args_t... args) {
        auto own_ptr = type_t::create(std::forward<args_t>(args)...);
        auto * to_return = own_ptr.get();
        types.emplace_back(std::move(own_ptr));
        return to_return;
    }

    type_ptr type_context::find_prim_type(ast::prim_type::type type) {
        // We should always find a prim_type

        for (auto & type_ptr : types) {
            if (auto * ptr = dynamic_cast<ast::prim_type *>(type_ptr.get()); ptr != nullptr) {
                if (ptr->inner() == type) { return ptr; }
            }
        }

        return emplace_type<prim_type>(type);
    }

    type_ptr type_context::find_int_type(unsigned size) {
        // We should always find a int_type

        for (auto & type_ptr : types) {
            if (auto * ptr = dynamic_cast<ast::int_type *>(type_ptr.get()); ptr != nullptr) {
                if (ptr->size == size) { return ptr; }
            }
        }

        return emplace_type<int_type>(size);
    }

    type_ptr type_context::find_ptr_type(bool is_nullable, type_ptr pointed_to) {
        for (auto & type_ptr : types) {
            if (auto * ptr = dynamic_cast<ast::ptr_type *>(type_ptr.get()); ptr != nullptr) {
                if (ptr->nullable() == is_nullable and ptr->pointed_to_type() == pointed_to) {
                    return ptr;
                }
            }
        }

        return is_nullable
                 ? static_cast<ptr_type *>(emplace_type<nullable_ptr_type>(pointed_to))
                 : static_cast<ptr_type *>(emplace_type<nonnullable_ptr_type>(pointed_to));
    }

    function_type * type_context::find_function_type(type_ptr return_type,
                                                     std::vector<type_ptr> && arg_types) {

        for (auto & type_ptr : types) {
            if (auto * ptr = dynamic_cast<ast::function_type *>(type_ptr.get()); ptr != nullptr) {
                if (ptr->return_type() != return_type) { continue; }
                if (ptr->arg_count() != arg_types.size()) { continue; }

                for (auto i = 0UL; i < ptr->arg_count(); ++i) {
                    if (ptr->arg(i) != arg_types[i]) { continue; }
                }

                return ptr;
            }
        }

        return emplace_type<function_type>(return_type, std::move(arg_types));
    }

    ast::struct_type *
    type_context::find_struct_type(std::string && name, const std::string & module_name,
                                   std::vector<struct_type::field_type> && fields) {

        for (auto & type_ptr : types) {
            if (auto * ptr = dynamic_cast<ast::struct_type *>(type_ptr.get()); ptr != nullptr) {
                if (ptr->containing_module_name() != module_name) { continue; }
                if (ptr->user_name() != name) { continue; }
                if (ptr->field_count() != fields.size()) { continue; }

                for (auto i = 0UL; i < ptr->field_count(); ++i) {
                    if (ptr->field(i) != fields[i]) { continue; }
                }

                return ptr;
            }
        }

        return emplace_type<struct_type>(std::move(name), module_name, std::move(fields));
    }

    type_ptr type_context::lookup_user_type(const std::string & name,
                                            const std::string & module_name) {
        for (auto & type_ptr : types) {
            if (auto * ptr = dynamic_cast<ast::user_type *>(type_ptr.get()); ptr != nullptr) {
                if (ptr->containing_module_name() == module_name and ptr->user_name() == name) {
                    return ptr;
                }
            }
        }

        return nullptr;
    }
} // namespace ast
