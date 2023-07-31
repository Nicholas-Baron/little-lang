#pragma once

#include "ast/type_context.hpp"
#include "control_flow/visitor.hpp"

#include <unordered_set>

namespace control_flow {
    class type_checker final : public visitor {

      public:
        type_checker(ast::type_context & ty_context);
        ~type_checker() noexcept override = default;

        non_copyable(type_checker);
        non_movable(type_checker);

        [[nodiscard]] bool checked_good() const noexcept { return not has_seen_error; }

      private:
        // clang-format off
#define expand_node_macro(x) virtual void visit(x &) override;
        all_cfg_nodes
#undef expand_node_macro

        // TODO: control_flow::node should store location data
        template<class... arg_t>
        void printError(const arg_t &... args);
        // clang-format on

        void arg_at(control_flow::intrinsic_call & call);
        void arg_count(control_flow::intrinsic_call & call);
        void syscall(control_flow::intrinsic_call & call);

        [[nodiscard]] const ast::type * current_return_type() const;

        ast::type_context & type_context;

        using instrinic_checker = void (type_checker::*)(intrinsic_call &);
        std::map<std::string, instrinic_checker> intrinsics;

        std::map<std::string, ast::type_ptr> bound_identifiers;

        struct node_info {
            ast::type_ptr type;
            bool can_widen;

            node_info(std::nullptr_t)
                : type{nullptr}
                , can_widen{} {}

            node_info(ast::type_ptr type, bool can_widen)
                : type{type}
                , can_widen{can_widen} {}

            [[nodiscard]] bool has_type() const { return type != nullptr; }

            [[nodiscard]] friend bool operator==(const node_info & lhs,
                                                 const node_info & rhs) noexcept {
                return lhs.type == rhs.type and lhs.can_widen == rhs.can_widen;
            }

            [[nodiscard]] friend bool operator!=(const node_info & lhs,
                                                 const node_info & rhs) noexcept {
                return not(lhs == rhs);
            }

            [[nodiscard]] friend bool operator<(const node_info & lhs,
                                                const node_info & rhs) noexcept {
                return lhs.type < rhs.type;
            }
        };

        node_info merge_types(const std::set<node_info> & input_types);
        void bind_identifier(std::string name, ast::type_ptr type);
        void bind_type(control_flow::node * value, node_info type_info);
        node_info find_type_of(control_flow::node * value) const;

        std::map<control_flow::node *, node_info> node_information;
        std::unordered_set<control_flow::node *> visited;
        const control_flow::function_start * current_function{nullptr};
        bool has_seen_error{false};
    };
} // namespace control_flow
