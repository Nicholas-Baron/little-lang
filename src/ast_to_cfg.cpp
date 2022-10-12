#include "ast_to_cfg.hpp"

#include "control_flow/node.hpp"

ast_to_cfg::ast_to_cfg()
    : result_cfg{std::make_unique<control_flow::graph>()} {}

void ast_to_cfg::visit(ast::node & node) { node.accept(*this); }
void ast_to_cfg::visit(ast::top_level & top_level) { top_level.accept(*this); }
void ast_to_cfg::visit(ast::stmt & stmt) { stmt.accept(*this); }
void ast_to_cfg::visit(ast::expr & expr) { expr.accept(*this); }

void ast_to_cfg::visit(ast::top_level_sequence & top_level_sequence) {
    // TODO: Load the imports
    assert(top_level_sequence.imports.empty());

    for (auto & item : top_level_sequence.items) { visit(*item); }
}

void ast_to_cfg::visit(ast::const_decl & /*const_decl*/) {
    assert(false and "Implement constant declarations");
}

void ast_to_cfg::visit(ast::binary_expr & binary_expr) {

    assert(not binary_expr.is_shortcircuiting() and "Implement short circuiting");

    auto * prev_node = result_cfg->previous_node();

    auto * cfg_lhs = get_value(*binary_expr.lhs, *this);
    cfg_lhs->flows_from(prev_node);
    prev_node = result_cfg->previous_node();

    auto * cfg_rhs = get_value(*binary_expr.rhs, *this);
    cfg_rhs->flows_from(prev_node);
    prev_node = result_cfg->previous_node();

    auto & cfg_expr = result_cfg->create<control_flow::binary_operation>();
    cfg_expr.lhs = cfg_lhs;
    cfg_expr.rhs = cfg_rhs;
    cfg_expr.flows_from(prev_node);
    store_result(&cfg_expr);
}

void ast_to_cfg::visit(ast::func_call_data & func_call_data) {
    std::vector<control_flow::node *> args;
    for (size_t i = 0; i < func_call_data.args_count(); ++i) {
        auto * arg = get_value(func_call_data.arg(i), *this);
        arg->flows_from(args.back());
        args.push_back(arg);
    }

    auto iter = seen_functions.find(func_call_data.name());
    if (iter == seen_functions.end()) { assert(false and "TODO: Make acutal error printout"); }

    auto * prev = args.back();

    auto & func_call = result_cfg->create<control_flow::function_call>(iter->second, args);
    func_call.flows_from(prev);
    return store_result(&func_call);
}

void ast_to_cfg::visit(ast::func_call_expr & func_call_expr) { return visit(func_call_expr.data); }
void ast_to_cfg::visit(ast::func_call_stmt & func_call_stmt) { return visit(func_call_stmt.data); }

void ast_to_cfg::visit(ast::func_decl & func_decl) {
    auto & func_start = result_cfg->create_root<control_flow::function_start>(
        func_decl.head.param_count(), func_decl.exported());
    current_function = &func_start;

    assert(seen_functions.find(func_decl.head.name()) == seen_functions.end());
    seen_functions.emplace(func_decl.head.name(), current_function);

    func_start.next = get_value(*func_decl.body, *this);
}

void ast_to_cfg::visit(ast::if_expr & /*unused*/) { assert(false and "TODO: Implement if_expr"); }
void ast_to_cfg::visit(ast::if_stmt & /*unused*/) { assert(false and "TODO: Implement if_stmt"); }
void ast_to_cfg::visit(ast::let_stmt & /*unused*/) { assert(false and "TODO: Implement let_stmt"); }

void ast_to_cfg::visit(ast::return_stmt & return_stmt) {

    auto * prev_node = result_cfg->previous_node();

    control_flow::node * return_value = nullptr;
    if (return_stmt.value != nullptr) {
        return_value = get_value(*return_stmt.value, *this);
        return_value->flows_from(prev_node);
        prev_node = return_value;
    }

    auto & return_node = result_cfg->create<control_flow::function_end>();
    return_node.value = return_value;
    return_node.flows_from(prev_node);
    return store_result(&return_node);
}

void ast_to_cfg::visit(ast::stmt_sequence & stmt_sequence) {
    auto * prev_node = result_cfg->previous_node();

    for (auto & stmt : stmt_sequence.stmts) {
        visit(*stmt);
        result_cfg->previous_node()->flows_from(prev_node);
        prev_node = result_cfg->previous_node();
    }

    return store_result(prev_node);
}

void ast_to_cfg::visit(ast::struct_decl & /*struct_decl*/) {
    assert(false and "Implement struct_decl visit");
}

void ast_to_cfg::visit(ast::struct_init & /*unused*/) {
    assert(false and "TODO: Implement struct_init");
}

void ast_to_cfg::visit(ast::typed_identifier & /*typed_identifier*/) {
    assert(false and "Implement typed_identifier visit");
}

void ast_to_cfg::visit(ast::unary_expr & /*unused*/) {
    assert(false and "TODO: Implement unary_expr");
}

void ast_to_cfg::visit(ast::user_val & /*unused*/) { assert(false and "TODO: Implement user_val"); }
