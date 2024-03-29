#pragma once

#include "ast/type_context.hpp"
#include "ast/visitor_base.hpp"
#include "control_flow/graph.hpp"
#include "control_flow/node.hpp"
#include "global_map.hpp"
#include "move_copy.hpp"
#include "utils/scoped_map.hpp"
#include "utils/value_getter.hpp"

#include <map>
#include <memory> // unique_ptr

/// This struct enforces that each statement has one beginning and endpoint.
struct basic_block {
    control_flow::node * beginning;
    control_flow::node * end;
    bool from_id_lookup{false};
};

class ast_to_cfg final
    : public ast::visitor_base
    , public value_getter<ast_to_cfg, ast::node, basic_block> {
  public:
    ast_to_cfg(ast::type_context & type_context);

    non_copyable(ast_to_cfg);
    movable(ast_to_cfg);

    ~ast_to_cfg() noexcept override = default;

    [[nodiscard]] std::unique_ptr<control_flow::graph> take_cfg() && noexcept {
        check_flow();
        result_cfg->set_program_name(std::move(current_module));
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

    void export_if_needed(const ast::top_level & ast_node,
                          control_flow::function_start * func_start);
    void import_item(const std::string & id, const std::string & mod);

    std::unique_ptr<control_flow::graph> result_cfg;

    std::map<std::string, control_flow::function_start *> seen_functions;
    std::map<std::string, ast::expr *> constants;
    std::map<std::string, ast::struct_type *> declared_structs;

    scoped_map<std::string, control_flow::node *> lets;
    control_flow::function_start * current_function{nullptr};

    ast::type_context * type_context;
    using global_map_t
        = global_map<std::string,
                     std::variant<std::monostate, ast::expr *, control_flow::function_start *>>;
    global_map_t globals;
    std::string current_module;
};
