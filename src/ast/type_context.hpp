#pragma once

#include "type.hpp"

namespace ast {
    class type_context final {
      public:
        type_context() = default;

        non_copyable(type_context);
        non_movable(type_context);

        ~type_context() noexcept = default;

        template<typename type_t, typename... arg_t>
        [[nodiscard]] auto * create_type(arg_t... args) {
            static_assert(std::is_class_v<type_t>);
            static_assert(std::is_convertible_v<type_t *, type *>);
            // Delagate to functions that will enforce each types uniqueness rules.
            if constexpr (std::is_same_v<type_t, ast::prim_type>) {
                // All primitive types should have been made by the constructor.
                static_assert(sizeof...(args) == 1);
                return find_prim_type(args...);
            } else if constexpr (std::is_same_v<type_t, ast::int_type>) {
                // All primitive types should have been made by the constructor.
                static_assert(sizeof...(args) == 1);
                return find_int_type(args...);
            } else if constexpr (std::is_convertible_v<type_t *, ptr_type *>) {
                // NOTE: The preceding check is broader than necessary,
                // as `ptr_type` could have more children in the future.
                // This could lead to some issues at that time.
                static_assert(sizeof...(args) == 1);
                return find_ptr_type(std::is_same_v<type_t, ast::nullable_ptr_type>, args...);
            } else if constexpr (std::is_same_v<type_t, ast::function_type>) {
                static_assert(sizeof...(args) > 0);
                static_assert(sizeof...(args) <= 2);
                return find_function_type(std::forward<arg_t>(args)...);
            } else if constexpr (std::is_same_v<type_t, ast::struct_type>) {
                static_assert(sizeof...(args) == 3);
                return find_struct_type(std::forward<arg_t>(args)...);
            }
        }

        [[nodiscard]] type_ptr lookup_user_type(const std::string & name,
                                                const std::string & module_name);

      private:
        [[nodiscard]] type_ptr find_prim_type(prim_type::type type);
        [[nodiscard]] type_ptr find_int_type(unsigned size);
        [[nodiscard]] type_ptr find_ptr_type(bool is_nullable, type_ptr pointed_to);
        [[nodiscard]] ast::function_type *
        find_function_type(type_ptr return_type, std::vector<type_ptr> && arg_types = {});

        [[nodiscard]] ast::struct_type *
        find_struct_type(std::string && name, const std::string & module_name,
                         std::vector<struct_type::field_type> && fields);

        template<typename type_t, typename... args_t>
        [[nodiscard]] type_t * emplace_type(args_t... args);

        std::vector<std::unique_ptr<ast::type>> types;
    };
} // namespace ast
