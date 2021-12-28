#ifndef TYPE_HPP
#define TYPE_HPP

#include "utils/move_copy.hpp"

#include <iosfwd> // ostream
#include <memory> // shared_ptr
#include <string>
#include <vector>

namespace ast {
    class type {
      public:
        // Items to mark the base class of an inheritance tree
        non_copyable(type);
        movable(type);
        virtual ~type() noexcept = default;

        [[nodiscard]] virtual bool is_pointer_type() const = 0;

      protected:
        type() = default;

      private:
        friend std::ostream & operator<<(std::ostream & lhs, const type & rhs) {
            rhs.print(lhs);
            return lhs;
        }
        virtual void print(std::ostream &) const = 0;

        [[nodiscard]] friend bool operator==(const type & lhs, const type & rhs) {
            return lhs.equals(rhs);
        }
        [[nodiscard]] friend bool operator!=(const type & lhs, const type & rhs) {
            return not(lhs == rhs);
        }
        [[nodiscard]] virtual bool equals(const type &) const = 0;
    };

    using type_ptr = std::shared_ptr<type>;

    class ptr_type : public type {
      public:
        type_ptr pointed_to;

        [[nodiscard]] virtual bool nullable() const noexcept = 0;
        [[nodiscard]] bool is_pointer_type() const final { return true; }

      protected:
        ptr_type(type_ptr inner)
            : pointed_to{std::move(inner)} {}
    };

    struct nullable_ptr_type final : public ptr_type {

        explicit nullable_ptr_type(type_ptr inner)
            : ptr_type{std::move(inner)} {}

        non_copyable(nullable_ptr_type);
        movable(nullable_ptr_type);
        ~nullable_ptr_type() final = default;

        [[nodiscard]] bool nullable() const final { return true; }

      private:
        void print(std::ostream & /*output*/) const final;
        [[nodiscard]] bool equals(const type & rhs) const final {
            const auto * rhs_cast = dynamic_cast<const nullable_ptr_type *>(&rhs);
            return rhs_cast != nullptr and *pointed_to == *rhs_cast->pointed_to;
        }
    };

    struct nonnullable_ptr_type final : public ptr_type {

        explicit nonnullable_ptr_type(type_ptr inner)
            : ptr_type{std::move(inner)} {}

        non_copyable(nonnullable_ptr_type);
        movable(nonnullable_ptr_type);
        ~nonnullable_ptr_type() final = default;

        [[nodiscard]] bool nullable() const final { return false; }

      private:
        void print(std::ostream & /*output*/) const final;

        [[nodiscard]] bool equals(const type & rhs) const final {
            const auto * rhs_cast = dynamic_cast<const nonnullable_ptr_type *>(&rhs);
            return rhs_cast != nullptr and *pointed_to == *rhs_cast->pointed_to;
        }
    };

    struct prim_type final : public type {
        enum class type { int32, boolean, character, float32, str, unit, null };
        explicit prim_type(type t)
            : prim{t} {}

        non_copyable(prim_type);
        movable(prim_type);
        ~prim_type() final = default;

        [[nodiscard]] bool is_pointer_type() const final {
            return prim == type::str or prim == type::null;
        }

        static inline const type_ptr int32
            = std::make_shared<ast::prim_type>(ast::prim_type::type::int32);
        static inline const type_ptr unit
            = std::make_shared<ast::prim_type>(ast::prim_type::type::unit);
        static inline const type_ptr float32
            = std::make_shared<ast::prim_type>(ast::prim_type::type::float32);
        static inline const type_ptr boolean
            = std::make_shared<ast::prim_type>(ast::prim_type::type::boolean);
        static inline const type_ptr str
            = std::make_shared<ast::prim_type>(ast::prim_type::type::str);
        static inline const type_ptr character
            = std::make_shared<ast::prim_type>(ast::prim_type::type::character);
        static inline const type_ptr null
            = std::make_shared<ast::prim_type>(ast::prim_type::type::null);

      private:
        void print(std::ostream & /*output*/) const final;

        [[nodiscard]] bool equals(const ast::type & rhs) const final {
            const auto * rhs_cast = dynamic_cast<const prim_type *>(&rhs);
            return rhs_cast != nullptr and prim == rhs_cast->prim;
        }

        type prim;
    };

    struct user_type final : public type {
        explicit user_type(std::string && name)
            : name{std::move(name)} {}

        non_copyable(user_type);
        movable(user_type);
        ~user_type() final = default;

        [[nodiscard]] bool is_pointer_type() const final { return false; }

        std::string name;

      private:
        void print(std::ostream & /*output*/) const final;

        [[nodiscard]] bool equals(const ast::type & rhs) const final {
            const auto * rhs_cast = dynamic_cast<const user_type *>(&rhs);
            return rhs_cast != nullptr and name == rhs_cast->name;
        }
    };

    struct function_type final : public type {

        explicit function_type(ast::type_ptr ret_type, std::vector<ast::type_ptr> && arg_types = {})
            : return_type{std::move(ret_type)}
            , arg_types{std::move(arg_types)} {}

        non_copyable(function_type);
        movable(function_type);
        ~function_type() final = default;

        [[nodiscard]] bool is_pointer_type() const final { return false; }

        ast::type_ptr return_type;
        std::vector<ast::type_ptr> arg_types;

      private:
        void print(std::ostream & /*output*/) const final;

        [[nodiscard]] bool equals(const ast::type & rhs) const final;
    };

} // namespace ast

#endif
