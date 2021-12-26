#ifndef TYPE_HPP
#define TYPE_HPP

#include "utils/move_copy.hpp"

#include <iosfwd> // ostream
#include <memory> // shared_ptr
#include <string>

namespace ast {
    class type {
      public:
        // Items to mark the base class of an inheritance tree
        non_copyable(type);
        movable(type);
        virtual ~type() noexcept = default;

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
        [[nodiscard]] virtual bool equals(const type &) const = 0;
    };

    using type_ptr = std::shared_ptr<type>;

    class ptr_type : public type {
      public:
        type_ptr pointed_to;

        [[nodiscard]] virtual bool nullable() const noexcept = 0;

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
        enum class type { int32, boolean, character, float32, str, unit };
        explicit prim_type(type t)
            : prim{t} {}

        non_copyable(prim_type);
        movable(prim_type);
        ~prim_type() final = default;

        type prim;

        static inline type_ptr int32
            = std::make_shared<ast::prim_type>(ast::prim_type::type::int32);
        static inline type_ptr unit = std::make_shared<ast::prim_type>(ast::prim_type::type::unit);
        static inline type_ptr float32
            = std::make_shared<ast::prim_type>(ast::prim_type::type::float32);
        static inline type_ptr boolean
            = std::make_shared<ast::prim_type>(ast::prim_type::type::boolean);
        static inline type_ptr str = std::make_shared<ast::prim_type>(ast::prim_type::type::str);
        static inline type_ptr character
            = std::make_shared<ast::prim_type>(ast::prim_type::type::character);

      private:
        void print(std::ostream & /*output*/) const final;

        [[nodiscard]] bool equals(const ast::type & rhs) const final {
            const auto * rhs_cast = dynamic_cast<const prim_type *>(&rhs);
            return rhs_cast != nullptr and prim == rhs_cast->prim;
        }
    };

    struct user_type final : public type {
        explicit user_type(std::string && name)
            : name{std::move(name)} {}

        non_copyable(user_type);
        movable(user_type);
        ~user_type() final = default;

        std::string name;

      private:
        void print(std::ostream & /*output*/) const final;

        [[nodiscard]] bool equals(const ast::type & rhs) const final {
            const auto * rhs_cast = dynamic_cast<const user_type *>(&rhs);
            return rhs_cast != nullptr and name == rhs_cast->name;
        }
    };
} // namespace ast

#endif
