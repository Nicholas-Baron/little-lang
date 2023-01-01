#ifndef CFG_TYPE_CHECKER_HPP
#define CFG_TYPE_CHECKER_HPP

#include "control_flow/visitor.hpp"

#include <unordered_set>

namespace control_flow {
    class type_checker final : public visitor {

      public:
        type_checker();
        ~type_checker() noexcept override = default;

        non_copyable(type_checker);
        movable(type_checker);

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

        void syscall(control_flow::intrinsic_call & call);

        void bind_identifier(std::string name, ast::type * type);
        void bind_type(control_flow::node * value, ast::type * type);
        ast::type * find_type_of(control_flow::node * value) const;

        using instrinic_checker = void (type_checker::*)(intrinsic_call &);
        std::map<std::string, instrinic_checker> intrinsics;

        std::map<std::string, ast::type *> bound_identifiers;
        std::map<control_flow::node *, ast::type *> node_type;
        std::unordered_set<control_flow::node *> visited;
        const ast::type * current_return_type{nullptr};
        bool has_seen_error{false};
    };
} // namespace control_flow

#endif
