#ifndef AST_TYPE_CONTEXT_HPP
#define AST_TYPE_CONTEXT_HPP

#include "type.hpp"

namespace ast {
    class type_context final {
      public:
        type_context();

        non_copyable(type_context);
        non_movable(type_context);

        ~type_context() noexcept = default;

        template<typename type_t, typename... arg_t>
        [[nodiscard]] type_ptr create_type(arg_t... args) {
            // Delagate to functions that will enforce each types uniqueness rules.
            if constexpr (std::is_same_v<type_t, ast::prim_type>) {
                // All primitive types should have been made by the constructor.
                static_assert(sizeof...(args) == 1);
                return find_prim_type(args...);
                // TODO: Fix formatting
            } else if constexpr (
                std::is_same_v<
                    type_t,
                    ast::nonnullable_ptr_type> or std::is_same_v<type_t, ast::nullable_ptr_type>) {
                static_assert(sizeof...(args) == 1);
                return find_ptr_type(std::is_same_v<type_t, ast::nullable_ptr_type>, args...);
            } else if constexpr (std::is_same_v<type_t, ast::function_type>) {
                static_assert(sizeof...(args) > 0);
                static_assert(sizeof...(args) <= 2);
                // TODO: use std::forward
                return find_function_type(args...);
            } else if constexpr (std::is_same_v<type_t, ast::struct_type>) {
                static_assert(sizeof...(args) == 3);
                return find_struct(args...);
            }
        }

        [[nodiscard]] type_ptr lookup_user_type(const std::string & name,
                                                const std::string & module_name);

      private:
        [[nodiscard]] type_ptr find_prim_type(prim_type::type type);
        [[nodiscard]] type_ptr find_ptr_type(bool is_nullable, type_ptr pointed_to);
        [[nodiscard]] type_ptr find_function_type(type_ptr return_type,
                                                  std::vector<type_ptr> && arg_types = {});
        [[nodiscard]] type_ptr find_struct_type(std::string && name,
                                                const std::string & module_name,
                                                std::vector<struct_type::field_type> && fields);

        std::vector<type_ptr> types;
    };
} // namespace ast

#endif
