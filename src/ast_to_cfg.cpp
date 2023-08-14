#include "ast_to_cfg.hpp"

#include "control_flow/node.hpp"
#include "intrinsics.hpp"
#include "string_utils.hpp"

#include <cassert>
#include <iostream> // cout

ast_to_cfg::ast_to_cfg(ast::type_context & type_context)
    : result_cfg{std::make_unique<control_flow::graph>()}
    , type_context{&type_context} {}

struct link {
    control_flow::node * node;
    control_flow::node * next;
};

static bool is_link_valid(control_flow::node * node, control_flow::node * next) {

    if (node == nullptr and next == nullptr) {
        std::cout << "Link of null to null found. Ignoring for now\n";
        return false;
    }

    if (node == nullptr) {
        std::cout << "Found null node for the next of " << typeid(*next).name() << std::endl;
        return false;
    }

    if (next == nullptr) {
        std::cout << "Found null next for node " << typeid(*node).name() << std::endl;
        return false;
    }

    return true;
}

#ifdef DEBUG
static std::string print_constant(const control_flow::constant * value) {
    return std::visit(
        [](auto & arg) -> std::string {
            if constexpr (std::is_same_v<std::decay_t<decltype(arg)>, std::monostate>) {
                return "null";
            } else if constexpr (std::is_same_v<std::decay_t<decltype(arg)>, std::string>) {
                return arg;
            } else {
                return std::to_string(arg);
            }
        },
        value->value);
}

static void print_graphviz(const std::vector<link> & links) {

    std::cout << "digraph {\n";
    std::set<std::string> seen;
    auto label_if_needed = [&seen](const auto & value, auto * node) {
        if (seen.find(value) == seen.end()) {
            auto label = std::stringstream{} << typeid(*node).name();

            if (auto * constant = dynamic_cast<control_flow::constant *>(node);
                constant != nullptr) {
                label << ' ' << print_constant(constant);
            } else if (auto * func_start = dynamic_cast<control_flow::function_start *>(node);
                       func_start != nullptr) {
                label << ' ' << func_start->name;
            } else if (auto * intrinsic_call = dynamic_cast<control_flow::intrinsic_call *>(node);
                       intrinsic_call != nullptr) {
                label << ' ' << intrinsic_call->name;
            } else if (auto * function_call = dynamic_cast<control_flow::function_call *>(node);
                       function_call != nullptr) {
                label << ' ' << function_call->callee->name;
            }

            std::cout << value << " [label=\"" << label.str() << "\"]\n";
        }

        seen.emplace(value);
    };

    for (const auto & link : links) {
        auto start = (std::stringstream{} << '\"' << std::hex << std::setw(sizeof(void *) * 2)
                                          << std::setfill('0')
                                          << reinterpret_cast<std::uintptr_t>(link.node) << '\"')
                         .str();

        auto end = (std::stringstream{} << '\"' << std::hex << std::setw(sizeof(void *) * 2)
                                        << std::setfill('0')
                                        << reinterpret_cast<std::uintptr_t>(link.next) << '\"')
                       .str();

        label_if_needed(start, link.node);
        label_if_needed(end, link.next);
        std::cout << start << " -> " << end << "[label=next];\n";
    }
    std::cout << '}' << std::endl;
}
#endif

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

        if (auto * un_op = dynamic_cast<control_flow::unary_operation *>(node); un_op != nullptr) {
            found_links.push_back({un_op->previous, un_op});
            return;
        }

        if (auto * bin_op = dynamic_cast<control_flow::binary_operation *>(node);
            bin_op != nullptr) {
            found_links.push_back({bin_op->previous, bin_op});
            return;
        }

        if (auto * func_call = dynamic_cast<control_flow::function_call *>(node);
            func_call != nullptr) {
            found_links.push_back({func_call->previous, func_call});
            return;
        }

        if (auto * call = dynamic_cast<control_flow::intrinsic_call *>(node); call != nullptr) {
            found_links.push_back({call->previous, call});
            return;
        }

        if (auto * phi = dynamic_cast<control_flow::phi *>(node); phi != nullptr) {
            for (auto * prev : phi->previous) { found_links.push_back({prev, phi}); }
            return;
        }

        if (auto * struct_init = dynamic_cast<control_flow::struct_init *>(node);
            struct_init != nullptr) {
            found_links.push_back({struct_init->previous, struct_init});
            return;
        }

        if (auto * member_access = dynamic_cast<control_flow::member_access *>(node);
            member_access != nullptr) {
            found_links.push_back({member_access->previous, member_access});
            return;
        }

        std::cout << "Previous reading for " << typeid(*node).name() << " has not been implemented"
                  << std::endl;
        assert(false);
    });

#ifdef DEBUG
    print_graphviz(found_links);
#endif

    // Use the map to build the next chain
    for (const auto & link : found_links) {

        auto * node = link.node;
        auto * next = link.next;

        if (is_link_valid(node, next)) { node->flows_to(next); }
    }
}

void ast_to_cfg::export_if_needed(const ast::top_level & ast_node,
                                  control_flow::function_start * func_start) {
    if (not ast_node.exported()) { return; }

    if (const auto * const_decl = dynamic_cast<const ast::const_decl *>(&ast_node);
        const_decl != nullptr) {
        assert(func_start == nullptr);
        globals.add(current_module, const_decl->name_and_type.name(), const_decl->expr.get());
    } else if (const auto * func_decl = dynamic_cast<const ast::func_decl *>(&ast_node);
               func_decl != nullptr) {
        assert(func_start != nullptr);
        globals.add(current_module, func_start->name, func_start);
    } else {
        assert(false);
    }
}

void ast_to_cfg::visit(ast::node & node) { node.accept(*this); }

void ast_to_cfg::visit(ast::top_level & top_level) { top_level.accept(*this); }

void ast_to_cfg::visit(ast::stmt & stmt) { stmt.accept(*this); }

void ast_to_cfg::visit(ast::expr & expr) { expr.accept(*this); }

void ast_to_cfg::import_item(const std::string & id, const std::string & mod) {
    if (auto value = globals.lookup(mod, id); value != decltype(globals)::empty_value()) {
        if (auto * const * const_expr = std::get_if<ast::expr *>(&value); const_expr != nullptr) {
            constants.emplace(id, *const_expr);
        } else if (auto * const * func_start = std::get_if<control_flow::function_start *>(&value);
                   func_start != nullptr) {
            seen_functions.emplace(id, *func_start);
        } else {
            assert(false);
        }
        // TODO: Mark that id has been imported. Useful for pseudo-modules
    } else if (not is_intrinsic(mod, id)) {
        std::cout << "Could not find " << id << " in module " << mod << "\nIt may not be exported."
                  << std::endl;
        assert(false);
    }
}

void ast_to_cfg::visit(ast::top_level_sequence & top_level_sequence) {
    for (auto & [mod, ids] : top_level_sequence.imports) {
        for (auto & id : ids) { import_item(id, mod); }
    }

    current_module = top_level_sequence.filename;
    for (auto & item : top_level_sequence.items) { visit(*item); }
    seen_functions.clear();
    constants.clear();
}

void ast_to_cfg::visit(ast::const_decl & const_decl) {

    auto name = const_decl.name_and_type.name();

    if (constants.find(name) != constants.end()) {
        std::cout << "Found two constants named " << name << "\nSecond declaraction @ "
                  << const_decl.location() << std::endl;
        assert(false);
    }

    constants.emplace(name, const_decl.expr.get());
    export_if_needed(const_decl, nullptr);
}

void ast_to_cfg::visit(ast::binary_expr & binary_expr) {

    auto cfg_lhs = get_value(*binary_expr.lhs, *this);

    if (binary_expr.is_shortcircuiting()) {
        auto & shorting_node = result_cfg->create<control_flow::branch>(cfg_lhs.end);
        shorting_node.flows_from(cfg_lhs.end);
        shorting_node.source_location = binary_expr.location();

        auto cfg_rhs = get_value(*binary_expr.rhs, *this);

        assert(binary_expr.op == operation::binary::bool_or
               or binary_expr.op == operation::binary::bool_and);

        auto & lhs_constant = result_cfg->create<control_flow::constant>(
            binary_expr.op == operation::binary::bool_or, literal_type::boolean);
        lhs_constant.flows_from(&shorting_node);

        shorting_node.true_case
            = (binary_expr.op == operation::binary::bool_and) ? cfg_rhs.beginning : &lhs_constant;

        shorting_node.false_case
            = (binary_expr.op == operation::binary::bool_or) ? cfg_rhs.beginning : &lhs_constant;

        auto & join_node = result_cfg->create<control_flow::phi>();
        join_node.flows_from(&lhs_constant);
        join_node.flows_from(cfg_rhs.end);
        join_node.source_location = binary_expr.location();

        return store_result({cfg_lhs.beginning, &join_node});
    }

    if (binary_expr.op == operation::binary::member_access) {

        auto * previous_node = result_cfg->previous_node();

        auto * member_ast = dynamic_cast<ast::user_val *>(binary_expr.rhs.get());
        assert(member_ast != nullptr);
        assert(member_ast->val_type == literal_type::identifier);

        auto & cfg_access
            = result_cfg->create<control_flow::member_access>(cfg_lhs.end, member_ast->val);
        // NOTE: `lhs` could be merged with `previous`
        cfg_access.flows_from(previous_node);
        cfg_access.source_location = binary_expr.location();
        return store_result(
            {cfg_lhs.from_id_lookup ? &cfg_access : cfg_lhs.beginning, &cfg_access});
    }

    auto cfg_rhs = get_value(*binary_expr.rhs, *this);
    cfg_rhs.beginning->flows_from(cfg_lhs.end);

    auto & cfg_expr = result_cfg->create<control_flow::binary_operation>(
        cfg_lhs.end, binary_expr.op, cfg_rhs.end);
    cfg_expr.flows_from(cfg_expr.rhs);
    cfg_expr.source_location = binary_expr.location();
    return store_result({cfg_lhs.beginning, &cfg_expr});
}

void ast_to_cfg::visit(ast::func_call_data & func_call_data) {
    auto * prev_node = result_cfg->previous_node();
    control_flow::node * first_arg = nullptr;

    std::vector<control_flow::node *> args;
    for (size_t i = 0; i < func_call_data.args_count(); ++i) {
        auto arg = get_value(func_call_data.arg(i), *this);
        if (arg.beginning != nullptr and not arg.from_id_lookup) {
            arg.beginning->flows_from(prev_node);
            prev_node = arg.end;
            if (first_arg == nullptr) { first_arg = arg.beginning; }
        }
        args.push_back(arg.end);
    }

    auto iter = seen_functions.find(func_call_data.name());
    if (iter == seen_functions.end()) {
        auto * possible_intrinsic
            = generate_ast_intrinsic(*result_cfg, func_call_data.name(), std::move(args));

        if (possible_intrinsic != nullptr) {
            assert(prev_node != nullptr);
            assert(prev_node != possible_intrinsic);
            possible_intrinsic->flows_from(prev_node);
            possible_intrinsic->source_location = func_call_data.location();
            return store_result(
                {first_arg != nullptr ? first_arg : possible_intrinsic, possible_intrinsic});
        }

        assert(false and "TODO: Make acutal error printout");
    }

    auto & func_call = result_cfg->create<control_flow::function_call>(iter->second, args);
    func_call.flows_from(prev_node);
    func_call.source_location = func_call_data.location();
    return store_result({first_arg != nullptr ? first_arg : &func_call, &func_call});
}

void ast_to_cfg::visit(ast::func_call_expr & func_call_expr) { return visit(func_call_expr.data); }

void ast_to_cfg::visit(ast::func_call_stmt & func_call_stmt) { return visit(func_call_stmt.data); }

void ast_to_cfg::visit(ast::func_decl & func_decl) {
    auto & func_start = result_cfg->create_root<control_flow::function_start>(
        func_decl.name, func_decl.param_count(), func_decl.exported(), func_decl.func_type);
    current_function = &func_start;
    func_start.source_location = func_decl.location();

    func_start.parameter_names.reserve(func_start.arg_count);
    for (auto i = 0UL; i < func_start.arg_count; ++i) {
        func_start.parameter_names.emplace_back(func_decl.params[i].name());
    }

    assert(seen_functions.find(func_decl.name) == seen_functions.end());
    seen_functions.emplace(func_decl.name, current_function);

    auto body = get_value(*func_decl.body, *this);
    if (body.beginning != nullptr) { body.beginning->flows_from(&func_start); }

    if (auto * previous_node = body.end != nullptr ? body.end : &func_start;
        dynamic_cast<control_flow::function_end *>(previous_node) == nullptr) {
        auto & func_end = result_cfg->create<control_flow::function_end>();
        func_end.flows_from(previous_node);
    }

    export_if_needed(func_decl, &func_start);
}

void ast_to_cfg::visit(ast::if_expr & if_expr) {
    auto * previous_node = result_cfg->previous_node();

    auto condition = get_value(*if_expr.condition, *this);
    auto & branch = result_cfg->create<control_flow::branch>(condition.end);
    condition.beginning->flows_from(previous_node);
    branch.source_location = if_expr.location();

    branch.flows_from(condition.end);

    auto true_case = get_value(*if_expr.then_case, *this);
    branch.true_case = true_case.beginning;
    branch.true_case->flows_from(&branch);

    auto false_case = get_value(*if_expr.else_case, *this);
    branch.false_case = false_case.beginning;
    branch.false_case->flows_from(&branch);

    auto & join_node = result_cfg->create<control_flow::phi>();
    join_node.flows_from(true_case.end);
    join_node.flows_from(false_case.end);

    return store_result({condition.beginning, &join_node});
}

void ast_to_cfg::visit(ast::if_stmt & if_stmt) {
    auto * previous_node = result_cfg->previous_node();

    auto condition_value = get_value(*if_stmt.condition, *this);
    if (condition_value.beginning != previous_node) {
        condition_value.beginning->flows_from(previous_node);
    }

    auto & branch = result_cfg->create<control_flow::branch>(condition_value.end);
    branch.flows_from(condition_value.end);
    branch.source_location = if_stmt.location();

    auto true_case = get_value(*if_stmt.true_branch, *this);
    branch.true_case = true_case.beginning;
    branch.true_case->flows_from(&branch);

    basic_block result{condition_value.beginning, &branch};

    // Check if a phi node is needed
    if (dynamic_cast<control_flow::function_end *>(true_case.end) != nullptr) {
        if (if_stmt.else_branch != nullptr) {
            auto false_case = get_value(*if_stmt.else_branch, *this);
            branch.false_case = false_case.beginning;
            branch.false_case->flows_from(&branch);
            result.end = false_case.end;
        }
        return store_result(result);
    }

    auto & join_node = result_cfg->create<control_flow::phi>();
    join_node.flows_from(true_case.end);
    result.end = &join_node;

    if (if_stmt.else_branch != nullptr) {
        auto false_case = get_value(*if_stmt.else_branch, *this);
        branch.false_case = false_case.beginning;
        branch.false_case->flows_from(&branch);
        join_node.flows_from(false_case.end);
    } else {
        join_node.flows_from(&branch);
    }

    assert(join_node.previous.size() >= 2);

    return store_result(result);
}

void ast_to_cfg::visit(ast::let_stmt & let_stmt) {
    auto value = get_value(*let_stmt.value, *this);
    lets.add_to_current_scope(let_stmt.name_and_type.name(), value.end);

    return store_result(value);
}

void ast_to_cfg::visit(ast::return_stmt & return_stmt) {

    std::optional<basic_block> return_value;
    if (return_stmt.value != nullptr) { return_value = get_value(*return_stmt.value, *this); }

    auto & return_node = result_cfg->create<control_flow::function_end>();
    return_node.source_location = return_stmt.location();
    if (return_value.has_value()) {
        return_node.value = return_value->end;
        if (not return_value->from_id_lookup) {
            return_node.flows_from(return_value->end);
            return store_result({return_value->beginning, &return_node});
        }
    }
    return store_result({&return_node, &return_node});
}

void ast_to_cfg::visit(ast::stmt_sequence & stmt_sequence) {

    lets.add_scope();
    basic_block result{nullptr, nullptr};

    for (auto & stmt : stmt_sequence.stmts) {
        auto stmt_node = get_value(*stmt, *this);
        if (result.end != nullptr and stmt_node.beginning != result.end) {
            stmt_node.beginning->flows_from(result.end);
        }
        result.end = stmt_node.end;

        if (result.beginning == nullptr) { result.beginning = stmt_node.beginning; }
    }

    lets.remove_scope();
    return store_result(result);
}

void ast_to_cfg::visit(ast::struct_decl & struct_decl) {
    std::vector<ast::struct_type::field_type> fields;

    fields.reserve(struct_decl.fields.size());
    for (auto & typed_id : struct_decl.fields) {
        fields.emplace_back(typed_id.name(), typed_id.type());
    }

    auto * found_type = type_context->create_type<ast::struct_type>(
        struct_decl.name, current_module, std::move(fields));

    assert(found_type == struct_decl.type);

    declared_structs.emplace(struct_decl.name, found_type);
}

void ast_to_cfg::visit(ast::struct_init & struct_init) {
    auto * previous_node = result_cfg->previous_node();

    auto & cfg_struct_init = result_cfg->create<control_flow::struct_init>();
    cfg_struct_init.source_location = struct_init.location();

    auto * struct_type = declared_structs[struct_init.type_name];
    if (struct_type == nullptr) { assert(false); }

    cfg_struct_init.result_type = struct_type;
    struct_init.type = struct_type;

    basic_block result{nullptr, &cfg_struct_init};

    for (auto & [field, value] : struct_init.initializers) {
        auto cfg_value = get_value(*value, *this);

        cfg_value.beginning->flows_from(previous_node);
        previous_node = cfg_value.end;

        if (result.beginning == nullptr) { result.beginning = cfg_value.beginning; }

        cfg_struct_init.fields.emplace(field, cfg_value.end);
    }

    if (result.beginning == nullptr) { result.beginning = &cfg_struct_init; }

    cfg_struct_init.flows_from(previous_node);
    return store_result(result);
}

void ast_to_cfg::visit(ast::typed_identifier & /*typed_identifier*/) {
    assert(false and "Implement typed_identifier visit");
}

void ast_to_cfg::visit(ast::unary_expr & unary_expr) {
    auto * previous_node = result_cfg->previous_node();

    auto operand = get_value(*unary_expr.expr, *this);
    if (not operand.from_id_lookup) { operand.beginning->flows_from(previous_node); }

    auto & unary_op = result_cfg->create<control_flow::unary_operation>(operand.end, unary_expr.op);
    unary_op.source_location = unary_expr.location();
    if (not operand.from_id_lookup) { unary_op.flows_from(unary_op.operand); }

    return store_result({operand.from_id_lookup ? &unary_op : operand.beginning, &unary_op});
}

static bool convert_to_bool(const std::string & value) {
    static const std::map<std::string, bool> values{
        {"true",  true },
        {"True",  true },
        {"TRUE",  true },
        {"false", false},
        {"False", false},
        {"FALSE", false},
    };

    auto iter = values.find(value);
    assert(iter != values.end());
    return iter->second;
}

void ast_to_cfg::visit(ast::user_val & user_val) {

    if (user_val.val_type == literal_type::identifier) {
        for (auto scope : lets) {
            if (auto iter = scope.find(user_val.val); iter != scope.end()) {
                return store_result({iter->second, iter->second, true});
            }
        }

        if (auto iter = constants.find(user_val.val); iter != constants.end()) {
            return visit(*iter->second);
        }
    }

    auto * prev_node = result_cfg->previous_node();

    control_flow::constant::value_variant_t value;
    switch (user_val.val_type) {
    case literal_type::null:
        break;
    case literal_type::identifier:
        value = user_val.val;
        break;
    case literal_type::integer:
        // TODO: Define our own string to int conversion
        value = std::stoi(user_val.val, nullptr, 0);
        break;
    case literal_type::floating:
        assert(false);
        break;
    case literal_type::character:
        switch (user_val.val.size()) {
        case 3:
            value = user_val.val.at(1);
            assert(std::holds_alternative<char>(value));
            break;
        case 4:
            assert(user_val.val.at(1) == '\\');
            switch (user_val.val.at(2)) {
            case '0':
                value = '\0';
                break;
            case 'n':
                value = '\n';
                break;
            }
            if (std::holds_alternative<char>(value)) { break; }
            [[fallthrough]];
        default:
            std::cout << "Cannot interpret " << user_val.val << " as a character" << std::endl;
            assert(false);
        }
        break;
    case literal_type::boolean:
        value = convert_to_bool(user_val.val);
        break;
    case literal_type::string:
        value = unquote(user_val.val);
        break;
    }

    auto & value_node
        = result_cfg->create<control_flow::constant>(std::move(value), user_val.val_type);
    value_node.source_location = user_val.location();
    value_node.type = user_val.type;
    value_node.flows_from(prev_node);

    return store_result({&value_node, &value_node});
}
