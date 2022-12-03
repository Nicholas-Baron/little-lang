#include "ast_to_cfg.hpp"

#include "ast/expr_nodes.hpp"
#include "control_flow/node.hpp"
#include "string_utils.hpp"

#include <iostream>

ast_to_cfg::ast_to_cfg()
    : result_cfg{std::make_unique<control_flow::graph>()} {}

struct link {
    control_flow::node * node;
    control_flow::node * next;
};

static void link_nodes(const std::vector<link> & links) {
    // Use the map to build the next chain
    for (const auto & link : links) {

        const auto & node = link.node;
        const auto & next = link.next;

        if (auto * func_start = dynamic_cast<control_flow::function_start *>(node);
            func_start != nullptr) {
            func_start->next = next;
            continue;
        }

        if (auto * func_end = dynamic_cast<control_flow::function_end *>(node);
            func_end != nullptr) {
            assert(false and "function_end should never be the previous of another node");
        }

        if (auto * value = dynamic_cast<control_flow::constant *>(node); value != nullptr) {
            value->next = next;
            continue;
        }

        if (auto * branch = dynamic_cast<control_flow::branch *>(node); branch != nullptr) {

            // The node we are encountering is not already a next.
            if (branch->true_case == node or branch->false_case == node) { continue; }

            // Exactly 1 of the next branches is not set.
            assert(branch->true_case != nullptr xor branch->false_case != nullptr);
            if (branch->true_case == nullptr) {
                branch->true_case = node;
            } else {
                branch->false_case = node;
            }

            continue;
        }

        if (auto * bin_op = dynamic_cast<control_flow::binary_operation *>(node);
            bin_op != nullptr) {
            bin_op->next = next;
            continue;
        }

        std::cout << "Forward linking for " << typeid(*node).name() << " has not been implemented"
                  << std::endl;
        assert(false);
    }
}

void ast_to_cfg::check_flow() noexcept {
    std::vector<link> found_links;

    // Fill the map
    result_cfg->for_each_node([&](auto * node) {
        if (auto * func_start = dynamic_cast<control_flow::function_start *>(node);
            func_start != nullptr) {
            // No previous to read
            return;
        }

        if (auto * func_end = dynamic_cast<control_flow::function_end *>(node);
            func_end != nullptr) {
            found_links.push_back({func_end->previous, func_end});
            return;
        }

        if (auto * value = dynamic_cast<control_flow::constant *>(node); value != nullptr) {
            found_links.push_back({value->previous, value});
            return;
        }

        if (auto * branch = dynamic_cast<control_flow::branch *>(node); branch != nullptr) {
            found_links.push_back({branch->previous, branch});
            return;
        }

        if (auto * bin_op = dynamic_cast<control_flow::binary_operation *>(node);
            bin_op != nullptr) {
            found_links.push_back({bin_op->previous, bin_op});
            return;
        }

        std::cout << "Previous reading for " << typeid(*node).name() << " has not been implemented"
                  << std::endl;
        assert(false);
    });

    link_nodes(found_links);
}

void ast_to_cfg::visit(ast::node & node) { node.accept(*this); }
void ast_to_cfg::visit(ast::top_level & top_level) { top_level.accept(*this); }
void ast_to_cfg::visit(ast::stmt & stmt) { stmt.accept(*this); }
void ast_to_cfg::visit(ast::expr & expr) { expr.accept(*this); }

void ast_to_cfg::visit(ast::top_level_sequence & top_level_sequence) {
    // TODO: Load the imports
    assert(top_level_sequence.imports.empty());

    for (auto & item : top_level_sequence.items) { visit(*item); }
}

void ast_to_cfg::visit(ast::const_decl & const_decl) {

    auto name = const_decl.name_and_type.name();

    if (constants.find(name) != constants.end()) {
        std::cout << "Found two constants named " << name << "\nSecond declaraction @ "
                  << const_decl.location() << std::endl;
        assert(false);
    }

    constants.emplace(name, const_decl.expr.get());
}

void ast_to_cfg::visit(ast::binary_expr & binary_expr) {

    auto * prev_node = result_cfg->previous_node();

    auto * cfg_lhs = get_value(*binary_expr.lhs, *this);
    cfg_lhs->flows_from(prev_node);
    prev_node = result_cfg->previous_node();

    if (binary_expr.is_shortcircuiting()) {
        auto & shorting_node = result_cfg->create<control_flow::branch>();
        shorting_node.flows_from(prev_node);
        shorting_node.condition_value = cfg_lhs;

        auto * cfg_rhs = get_value(*binary_expr.rhs, *this);
        cfg_rhs->flows_from(&shorting_node);
        prev_node = result_cfg->previous_node();

        assert(binary_expr.op == ast::binary_expr::operand::bool_or
               or binary_expr.op == ast::binary_expr::operand::bool_and);

        shorting_node.true_case
            = (binary_expr.op == ast::binary_expr::operand::bool_and) ? cfg_rhs : nullptr;

        shorting_node.false_case
            = (binary_expr.op == ast::binary_expr::operand::bool_or) ? cfg_rhs : nullptr;

        return store_result(&shorting_node);
    }

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
    auto * prev_node = result_cfg->previous_node();

    std::vector<control_flow::node *> args;
    for (size_t i = 0; i < func_call_data.args_count(); ++i) {
        auto * arg = get_value(func_call_data.arg(i), *this);
        arg->flows_from(prev_node);
        args.push_back(arg);
        prev_node = arg;
    }

    auto iter = seen_functions.find(func_call_data.name());
    if (iter == seen_functions.end()) { assert(false and "TODO: Make acutal error printout"); }

    auto & func_call = result_cfg->create<control_flow::function_call>(iter->second, args);
    func_call.flows_from(prev_node);
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

    if (auto * previous_node = result_cfg->previous_node();
        dynamic_cast<control_flow::function_end *>(previous_node) == nullptr) {
        auto & func_end = result_cfg->create<control_flow::function_end>();
        func_end.flows_from(previous_node);
    }
}

void ast_to_cfg::visit(ast::if_expr & if_expr) {
    auto * previous_node = result_cfg->previous_node();

    auto & branch = result_cfg->create<control_flow::branch>();
    branch.condition_value = get_value(*if_expr.condition, *this);
    branch.condition_value->flows_from(previous_node);

    branch.flows_from(branch.condition_value);

    branch.true_case = get_value(*if_expr.then_case, *this);
    branch.true_case->flows_from(&branch);

    branch.false_case = get_value(*if_expr.else_case, *this);
    branch.false_case->flows_from(&branch);

    store_result(&branch);
}

void ast_to_cfg::visit(ast::if_stmt & if_stmt) {
    auto * previous_node = result_cfg->previous_node();

    auto & branch = result_cfg->create<control_flow::branch>();
    branch.condition_value = get_value(*if_stmt.condition, *this);
    branch.condition_value->flows_from(previous_node);

    branch.flows_from(branch.condition_value);

    branch.true_case = get_value(*if_stmt.true_branch, *this);
    branch.true_case->flows_from(&branch);

    if (if_stmt.else_branch != nullptr) {
        branch.false_case = get_value(*if_stmt.else_branch, *this);
        branch.false_case->flows_from(&branch);
    }

    store_result(&branch);
}

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
        auto * stmt_node = get_value(*stmt, *this);
        stmt_node->flows_from(prev_node);
        prev_node = stmt_node;
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

void ast_to_cfg::visit(ast::unary_expr & unary_expr) {
    auto * previous_node = result_cfg->previous_node();

    auto & unary_op = result_cfg->create<control_flow::unary_operation>();
    unary_op.operand = get_value(*unary_expr.expr, *this);
    unary_op.operand->flows_from(previous_node);
    unary_op.flows_from(unary_op.operand);

    switch (unary_expr.op) {
    case ast::unary_expr::operand::bool_not:
        unary_op.op = control_flow::unary_operation::operation::bool_not;
        break;
    case ast::unary_expr::operand::negate:
        unary_op.op = control_flow::unary_operation::operation::negate;
        break;
    case ast::unary_expr::operand::deref:
        unary_op.op = control_flow::unary_operation::operation::deref;
        break;
    }

    return store_result(&unary_op);
}

void ast_to_cfg::visit(ast::user_val & user_val) {

    auto * prev_node = result_cfg->previous_node();

    auto & value = result_cfg->create<control_flow::constant>();
    value.flows_from(prev_node);

    switch (user_val.val_type) {
    case ast::user_val::value_type::null:
        value.val_type = control_flow::constant::value_type::null;
        break;
    case ast::user_val::value_type::identifier:
        value.val_type = control_flow::constant::value_type::identifier;
        value.value = user_val.val;
        break;
    case ast::user_val::value_type::integer:
        value.val_type = control_flow::constant::value_type::integer;
        // TODO: Define our own string to int conversion
        value.value = std::stoi(user_val.val);
        break;
    case ast::user_val::value_type::floating:
        value.val_type = control_flow::constant::value_type::floating;
        assert(false);
        break;
    case ast::user_val::value_type::character:
        value.val_type = control_flow::constant::value_type::character;
        assert(false);
        break;
    case ast::user_val::value_type::boolean:
        value.val_type = control_flow::constant::value_type::boolean;
        assert(false);
        break;
    case ast::user_val::value_type::string:
        value.val_type = control_flow::constant::value_type::string;
        value.value = unquote(user_val.val);
        break;
    }

    store_result(&value);
}
