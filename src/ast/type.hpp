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
        non_movable(type);
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
    };

    using type_ptr = std::shared_ptr<type>;

    class ptr_type : public type {
      public:
        [[nodiscard]] virtual bool nullable() const noexcept = 0;
        [[nodiscard]] bool is_pointer_type() const final { return true; }

        [[nodiscard]] type_ptr pointed_to_type() const { return pointed_to; }

      protected:
        ptr_type(type_ptr inner)
            : pointed_to{std::move(inner)} {}

      private:
        type_ptr pointed_to;
    };

    struct nullable_ptr_type final : public ptr_type {

        static std::shared_ptr<nullable_ptr_type> create(type_ptr pointed_to_type);

        non_copyable(nullable_ptr_type);
        non_movable(nullable_ptr_type);
        ~nullable_ptr_type() final = default;

        [[nodiscard]] bool nullable() const final { return true; }

      private:
        explicit nullable_ptr_type(type_ptr inner)
            : ptr_type{std::move(inner)} {}

        void print(std::ostream & /*output*/) const final;
    };

    struct nonnullable_ptr_type final : public ptr_type {

        static std::shared_ptr<nonnullable_ptr_type> create(type_ptr pointed_to_type);

        non_copyable(nonnullable_ptr_type);
        non_movable(nonnullable_ptr_type);
        ~nonnullable_ptr_type() final = default;

        [[nodiscard]] bool nullable() const final { return false; }

      private:
        explicit nonnullable_ptr_type(type_ptr inner)
            : ptr_type{std::move(inner)} {}

        void print(std::ostream & /*output*/) const final;
    };

    struct prim_type final : public type {
        enum class type {
            int32,
            boolean,
            character,
            float32,
            str,
            unit,
        };

        non_copyable(prim_type);
        non_movable(prim_type);
        ~prim_type() final = default;

        [[nodiscard]] bool is_pointer_type() const final {
            // TODO: Should `str` still be a pointer type?
            return prim == type::str;
        }

        static const type_ptr int32;
        static const type_ptr unit;
        static const type_ptr float32;
        static const type_ptr boolean;
        static const type_ptr str;
        static const type_ptr character;

      private:
        explicit prim_type(type t)
            : prim{t} {}

        void print(std::ostream & /*output*/) const final;

        type prim;
    };

    struct user_type final : public type {

        static std::shared_ptr<user_type> create(std::string &&);

        non_copyable(user_type);
        non_movable(user_type);
        ~user_type() final = default;

        [[nodiscard]] bool is_pointer_type() const final { return false; }

      private:
        explicit user_type(std::string name)
            : name{std::move(name)} {}

        std::string name;

        void print(std::ostream & /*output*/) const final;
    };

    struct function_type final : public type {
        static std::shared_ptr<function_type> create(ast::type_ptr ret_type,
                                                     std::vector<ast::type_ptr> && arg_types = {});

        non_copyable(function_type);
        non_movable(function_type);
        ~function_type() final = default;

        [[nodiscard]] bool is_pointer_type() const final { return false; }

        [[nodiscard]] size_t arg_count() const noexcept { return arg_types.size(); }
        [[nodiscard]] type_ptr arg(size_t i) const { return arg_types[i]; }
        [[nodiscard]] type_ptr return_type() const { return ret_type; }

      private:
        explicit function_type(ast::type_ptr ret_type, std::vector<ast::type_ptr> && arg_types = {})
            : ret_type{std::move(ret_type)}
            , arg_types{std::move(arg_types)} {}

        ast::type_ptr ret_type;
        std::vector<ast::type_ptr> arg_types;

        void print(std::ostream & /*output*/) const final;
    };

} // namespace ast

#endif
