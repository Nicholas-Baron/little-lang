#ifndef AST_TO_CFG
#define AST_TO_CFG

#include "ast/visitor_base.hpp"
#include "control_flow/graph.hpp"
#include "control_flow/node.hpp"
#include "move_copy.hpp"
#include "utils/value_getter.hpp"

#include <map>
#include <memory> // unique_ptr

class ast_to_cfg final : public ast::visitor_base,
                         public value_getter<ast_to_cfg, ast::node, control_flow::node *> {
  public:
    ast_to_cfg();

    non_copyable(ast_to_cfg);
    movable(ast_to_cfg);

    ~ast_to_cfg() noexcept override = default;

    [[nodiscard]] std::unique_ptr<control_flow::graph> take_cfg() && noexcept {
        check_flow();
        return std::move(result_cfg);
    }

    // clang-format off
#define expand_node_macro(name) virtual void visit(ast::name &) override;
    ast_nodes
#undef expand_node_macro
  private:
    // Link the `next` pointers up.
    // The `previous` ones were done by `flows_from`
    void check_flow() noexcept;
    // clang-format on

    std::unique_ptr<control_flow::graph> result_cfg;

    std::map<std::string, const control_flow::function_start *> seen_functions;
    std::map<std::string, ast::expr *> constants;
    const control_flow::function_start * current_function{nullptr};
};

#endif
