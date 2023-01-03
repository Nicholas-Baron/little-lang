#ifndef TYPE_HPP
#define TYPE_HPP

#include <iosfwd> // ostream
#include <memory> // shared_ptr
#include <string>
#include <vector>

#include <move_copy.hpp>

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

        non_copyable(nullable_ptr_type);
        non_movable(nullable_ptr_type);
        ~nullable_ptr_type() final = default;

        [[nodiscard]] bool nullable() const noexcept final { return true; }

      private:
        static std::shared_ptr<nullable_ptr_type> create(type_ptr pointed_to_type);

        explicit nullable_ptr_type(type_ptr inner)
            : ptr_type{std::move(inner)} {}

        void print(std::ostream & /*output*/) const final;

        friend class type_context;
    };

    struct nonnullable_ptr_type final : public ptr_type {

        non_copyable(nonnullable_ptr_type);
        non_movable(nonnullable_ptr_type);
        ~nonnullable_ptr_type() final = default;

        [[nodiscard]] bool nullable() const noexcept final { return false; }

      private:
        static std::shared_ptr<nonnullable_ptr_type> create(type_ptr pointed_to_type);

        explicit nonnullable_ptr_type(type_ptr inner)
            : ptr_type{std::move(inner)} {}

        void print(std::ostream & /*output*/) const final;

        friend class type_context;
    };

    struct prim_type final : public type {
        enum class type {
            int32,
            boolean,
            character,
            float32,
            str,
            unit,
            null,
        };

        non_copyable(prim_type);
        non_movable(prim_type);
        ~prim_type() final = default;

        [[nodiscard]] bool is_pointer_type() const final {
            // TODO: Should `str` still be a pointer type?
            return prim == type::str or prim == type::null;
        }

        [[nodiscard]] type inner() const noexcept { return prim; }

      private:
        explicit prim_type(type prim)
            : prim{prim} {}

        void print(std::ostream & /*output*/) const final;

        type prim;

        // TODO: This class has no need to be friends with type_context
    };

    struct user_type : public type {

        non_copyable(user_type);
        non_movable(user_type);
        ~user_type() override = default;

        [[nodiscard]] bool is_pointer_type() const final { return false; }

        [[nodiscard]] const std::string & containing_module_name() const { return module_name; }
        [[nodiscard]] const std::string & user_name() const { return name; }

      protected:
        explicit user_type(std::string name, std::string module_name)
            : name{std::move(name)}
            , module_name{std::move(module_name)} {}

      private:
        std::string name;
        std::string module_name;
    };

    struct struct_type final : public user_type {

        using field_type = std::pair<std::string, ast::type_ptr>;

        non_copyable(struct_type);
        non_movable(struct_type);

        ~struct_type() final = default;

        [[nodiscard]] size_t field_count() const noexcept { return fields.size(); }
        [[nodiscard]] const field_type & field(size_t index) const { return fields[index]; }

      private:
        static std::shared_ptr<struct_type> create(std::string && name,
                                                   const std::string & module_name,
                                                   std::vector<field_type> && fields);

        struct_type(std::string && name, std::string module_name, std::vector<field_type> && fields)
            : user_type{std::move(name), std::move(module_name)}
            , fields{std::move(fields)} {}

        std::vector<field_type> fields;

        void print(std::ostream & /*output*/) const override;

        friend class type_context;
    };

    struct function_type final : public type {
        non_copyable(function_type);
        non_movable(function_type);
        ~function_type() final = default;

        [[nodiscard]] bool is_pointer_type() const final { return false; }

        [[nodiscard]] size_t arg_count() const noexcept { return arg_types.size(); }
        [[nodiscard]] type_ptr arg(size_t index) const { return arg_types[index]; }
        [[nodiscard]] type_ptr return_type() const { return ret_type; }

      private:
        static std::shared_ptr<function_type> create(ast::type_ptr ret_type,
                                                     std::vector<ast::type_ptr> && arg_types = {});

        explicit function_type(ast::type_ptr ret_type, std::vector<ast::type_ptr> && arg_types = {})
            : ret_type{std::move(ret_type)}
            , arg_types{std::move(arg_types)} {}

        ast::type_ptr ret_type;
        std::vector<ast::type_ptr> arg_types;

        void print(std::ostream & /*output*/) const final;

        friend class type_context;
    };

} // namespace ast

#endif
