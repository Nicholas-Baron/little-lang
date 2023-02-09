#include "type_checker.hpp"

#include "ast/type.hpp"
#include "node.hpp"
#include "operations.hpp"

#include <iostream>

namespace control_flow {

    type_checker::type_checker(ast::type_context & ty_context)
        : type_context{ty_context} {
        intrinsics.emplace("syscall", &type_checker::syscall);
    }

    template<class... arg_t>
    void type_checker::printError(const arg_t &... args) {
        std::stringstream to_print;

        (to_print << ... << args);

        std::cerr << to_print.str() << std::endl;
        has_seen_error = true;
    }

    void type_checker::bind_type(control_flow::node * value, ast::type_ptr type) {
        node_type.emplace(value, type);

        if (auto * binary_op = dynamic_cast<control_flow::binary_operation *>(value);
            binary_op != nullptr) {
            binary_op->result_type = type;
        } else if (auto * unary_op = dynamic_cast<control_flow::unary_operation *>(value);
                   unary_op != nullptr) {
            unary_op->result_type = type;
        }
    }

    void type_checker::bind_identifier(std::string name, ast::type * type) {
        bound_identifiers.emplace(std::move(name), type);
    }

    ast::type_ptr type_checker::find_type_of(control_flow::node * value) const {
        auto iter = node_type.find(value);
        return (iter != node_type.end()) ? iter->second : nullptr;
    }

    void type_checker::syscall(intrinsic_call & call) {
        // Syscalls can only take between 1 and 7 arguments.
        static constexpr auto max_syscall_args = 7U;
        if (const auto arg_count = call.arguments.size();
            arg_count == 0 or arg_count > max_syscall_args) {
            printError("syscalls can only take 1 to 7 arguments\nFound one with ", arg_count);
        }

        auto * int_type = type_context.create_type<ast::prim_type>(ast::prim_type::type::int32);

        bool first = true;
        std::vector<ast::type_ptr> arg_types;
        for (auto * arg : call.arguments) {
            auto * arg_type = find_type_of(arg);
            if (not arg_type->is_pointer_type() and arg_type != int_type) {
                printError("syscall can only take int or pointer arguments; found ", *arg_type);
            }

            // The first argmuent must always be a syscall number.
            if (first) {
                first = false;
                if (arg_type != int_type) {
                    printError("syscall must have an integer as its first argument; found ",
                               *arg_type);
                }
            }

            arg_types.push_back(arg_type);
        }

        call.type = type_context.create_type<ast::function_type>(int_type, std::move(arg_types));

        // TODO: syscalls can return pointers and 64 bit numbers
        bind_type(&call, int_type);
    }

    void type_checker::visit(function_start & func_start) {
        current_return_type = func_start.type->return_type();

        for (auto i = 0UL; i < func_start.arg_count; ++i) {
            bind_identifier(func_start.parameter_names[i], func_start.type->arg(i));
        }
        bind_identifier(func_start.name, func_start.type);
        visited.emplace(&func_start);

        func_start.next->accept(*this);

        for (auto i = 0UL; i < func_start.arg_count; ++i) {
            auto iter = bound_identifiers.find(func_start.parameter_names[i]);
            assert(iter != bound_identifiers.end());
            bound_identifiers.erase(iter);
        }
    }

    void type_checker::visit(binary_operation & binary_operation) {

        auto * lhs_type = find_type_of(binary_operation.lhs);
        assert(lhs_type != nullptr);

        if (auto * struct_type = dynamic_cast<ast::struct_type *>(lhs_type);
            struct_type != nullptr and binary_operation.op == operation::binary::member_access) {

            auto * identifier_node = dynamic_cast<control_flow::constant *>(binary_operation.rhs);
            assert(identifier_node != nullptr);

            assert(identifier_node->val_type == literal_type::identifier);
            auto identifier = std::get<std::string>(identifier_node->value);
            assert(not identifier.empty());

            for (auto field_index = 0UL; field_index < struct_type->field_count(); ++field_index) {
                auto field = struct_type->field(field_index);
                if (field.first == identifier) {
                    return bind_type(&binary_operation, field.second);
                }
            }

            return printError("Struct type ", struct_type->user_name(),
                              " does not have a field named ", identifier);
        }

        auto * rhs_type = find_type_of(binary_operation.rhs);
        assert(rhs_type != nullptr);

        auto * boolean_type
            = type_context.create_type<ast::prim_type>(ast::prim_type::type::boolean);
        if (operation::is_shortcircuiting(binary_operation.op)) {
            // Left and right must be booleans
            if (lhs_type != boolean_type) {
                printError("lhs expected to be boolean, found ", *lhs_type);
            }
            if (rhs_type != boolean_type) {
                printError("rhs expected to be boolean, found ", *rhs_type);
            }
            bind_type(&binary_operation, boolean_type);
        } else if (operation::is_comparison(binary_operation.op)) {
            if (lhs_type != rhs_type) {
                printError("Expected comparison operands to be of same type; found ", *lhs_type,
                           " and ", *rhs_type);
            }

            bind_type(&binary_operation, boolean_type);
        } else if (operation::is_arithmetic(binary_operation.op)) {
            auto * result_type = lhs_type;

            if (binary_operation.op == operation::binary::add
                and (lhs_type->is_pointer_type() or rhs_type->is_pointer_type())) {
                if (lhs_type->is_pointer_type() == rhs_type->is_pointer_type()) {
                    printError("Cannot add two pointers together");
                } else {
                    auto * non_ptr_type = lhs_type->is_pointer_type() ? rhs_type : lhs_type;
                    auto * ptr_type = lhs_type->is_pointer_type() ? lhs_type : rhs_type;

                    auto * int_type
                        = type_context.create_type<ast::prim_type>(ast::prim_type::type::int32);
                    if (non_ptr_type != int_type) {
                        printError("Expected ", *int_type, " to add with ", *ptr_type, "; found ",
                                   *non_ptr_type);
                    }

                    result_type = ptr_type;
                }
            } else if (lhs_type != rhs_type) {
                printError("Expected arithmetic operands to be of same type; found ", *lhs_type,
                           " and ", *rhs_type);
            }

            bind_type(&binary_operation, result_type);
        } else {
            assert(false);
        }

        visited.emplace(&binary_operation);
        binary_operation.next->accept(*this);
    }

    void type_checker::visit(branch & branch) {
        auto * cond_type = find_type_of(branch.condition_value);
        assert(cond_type != nullptr);

        auto * boolean_type
            = type_context.create_type<ast::prim_type>(ast::prim_type::type::boolean);
        if (cond_type != boolean_type) {
            printError("Expected condition to be of type ", *boolean_type, "; found ", *cond_type);
        }

        visited.emplace(&branch);
        branch.true_case->accept(*this);
        branch.false_case->accept(*this);
    }

    void type_checker::visit(constant & constant) {
        ast::type_ptr const_type = nullptr;
        auto get_type = [this](ast::prim_type::type prim) -> decltype(const_type) {
            return type_context.create_type<ast::prim_type>(prim);
        };

        switch (constant.val_type) {
        case literal_type::null:
            const_type = get_type(ast::prim_type::type::null);
            break;
        case literal_type::identifier:
            if (auto iter = bound_identifiers.find(std::get<std::string>(constant.value));
                iter != bound_identifiers.end()) {
                const_type = iter->second;
            } else {
                printError("Could not find the type of ", std::get<std::string>(constant.value));
            }
            break;
        case literal_type::integer:
            const_type = get_type(ast::prim_type::type::int32);
            break;
        case literal_type::floating:
            const_type = get_type(ast::prim_type::type::float32);
            break;
        case literal_type::character:
            const_type = get_type(ast::prim_type::type::character);
            break;
        case literal_type::boolean:
            const_type = get_type(ast::prim_type::type::boolean);
            break;
        case literal_type::string:
            const_type = get_type(ast::prim_type::type::str);
            break;
        }

        constant.type = const_type;
        visited.emplace(&constant);
        bind_type(&constant, const_type);

        constant.next->accept(*this);
    }

    void type_checker::visit(function_call & func_call) {
        const auto & func_name = func_call.callee->name;
        auto * expected_func_type = [this, &func_name]() -> ast::function_type * {
            auto iter = bound_identifiers.find(func_name);

            if (iter == bound_identifiers.end()) {
                printError("Could not find type of function ", func_name);
                return nullptr;
            }

            auto * to_return = dynamic_cast<ast::function_type *>(iter->second);

            if (to_return == nullptr) {
                printError("Expected a function type for ", func_name, "; found ", *iter->second,
                           ", which is not a function");
            }

            return to_return;
        }();

        if (expected_func_type == nullptr) { return; }

        if (expected_func_type->arg_count() != func_call.arguments.size()) {
            printError(func_name, " expects ", expected_func_type->arg_count(),
                       " arguments; found ", func_call.arguments.size());
        }

        auto args_to_check = std::min(expected_func_type->arg_count(), func_call.arguments.size());

        for (auto i = 0UL; i < args_to_check; ++i) {
            auto * actual_type = find_type_of(func_call.arguments[i]);
            auto * expected_type = expected_func_type->arg(i);

            if (actual_type == nullptr) {
                printError(func_name, " argument ", i, ": Could not find type");
                continue;
            }

            if (actual_type != expected_type) {
                printError(func_name, " argument ", i, ": Expected type ", *expected_type,
                           "; found ", *actual_type);
            }
        }

        bind_type(&func_call, expected_func_type->return_type());
        visited.emplace(&func_call);
        func_call.next->accept(*this);
    }

    void type_checker::visit(function_end & func_end) {
        assert(current_return_type != nullptr);

        auto * const actual_type
            = (func_end.value != nullptr)
                ? find_type_of(func_end.value)
                : type_context.create_type<ast::prim_type>(ast::prim_type::type::unit);

        if (current_return_type != actual_type) {
            printError("Expected a return expression with type ", *current_return_type,
                       "; found one with ", *actual_type);
        }

        visited.emplace(&func_end);
    }

    void type_checker::visit(intrinsic_call & intrinsic_call) {
        if (auto iter = intrinsics.find(intrinsic_call.name); iter != intrinsics.end()) {
            (this->*iter->second)(intrinsic_call);
        } else {
            printError("Could not type check intrinsic named ", intrinsic_call.name);
        }
    }

    void type_checker::visit(phi & phi) {
        // Only go to the next node if all previous nodes have been checked

        auto should_continue = true;
        for (auto * prev : phi.previous) {
            if (visited.find(prev) == visited.end()) {
                should_continue = false;
                break;
            }
        }

        if (should_continue) {

            // HACK: Sometimes, phi nodes are values and need a type.
            //       This will happen if all previous nodes have the same type.

            std::optional<ast::type_ptr> phi_type;
            for (auto * prev : phi.previous) {
                if (auto * prev_type = find_type_of(prev); not phi_type.has_value()) {
                    phi_type = prev_type;
                } else if (phi_type != prev_type) {
                    phi_type = nullptr;
                }
            }

            assert(phi_type.has_value());

            if (phi_type != nullptr) { bind_type(&phi, *phi_type); }

            visited.emplace(&phi);
            phi.next->accept(*this);
        }
    }

    void type_checker::visit(struct_init & struct_init) {

        // Check that all fields are initialized

        auto * struct_type = struct_init.result_type;

        for (auto field_index = 0UL; field_index < struct_type->field_count(); ++field_index) {
            auto [field_name, expected_type] = struct_type->field(field_index);

            auto iter = struct_init.fields.find(field_name);
            if (iter == struct_init.fields.end()) {
                printError("Field ", field_name, " is not initialized for struct of type ",
                           struct_type->user_name());
                continue;
            }

            if (auto * actual_type = find_type_of(iter->second); actual_type != expected_type) {
                printError("Expected type of ", *expected_type, " for field ", field_name,
                           " in struct ", struct_type->user_name(), "; Found expession with type ",
                           *actual_type);
            }
        }

        bind_type(&struct_init, struct_init.result_type);

        visited.emplace(&struct_init);
        struct_init.next->accept(*this);
    }

    void type_checker::visit(unary_operation & unary_operation) {
        auto * operand_type = find_type_of(unary_operation.operand);
        assert(operand_type != nullptr);

        auto * result_type = operand_type;

        auto is_operand_prim = [this, &operand_type](auto prim) -> bool {
            return operand_type == type_context.create_type<ast::prim_type>(prim);
        };

        switch (unary_operation.op) {
        case operation::unary::bool_not:
            if (not is_operand_prim(ast::prim_type::type::boolean)) {
                printError("Expected boolean for `!`; found", *operand_type);
            }
            break;
        case operation::unary::negate:
            if (not is_operand_prim(ast::prim_type::type::int32)
                and not is_operand_prim(ast::prim_type::type::float32)) {
                printError("Expected float or int for `-`; found", *operand_type);
            }
            break;
        case operation::unary::deref:
            if (not operand_type->is_pointer_type()) {
                printError("Expected a pointer for `*`; found", *operand_type);
            } else if (is_operand_prim(ast::prim_type::type::null)) {
                printError("Cannot deref a null literal");
            } else if (is_operand_prim(ast::prim_type::type::str)) {
                result_type
                    = type_context.create_type<ast::prim_type>(ast::prim_type::type::character);
            } else {
                // TODO: Enforce nonnullable_ptr_type
                auto * ptr_type = dynamic_cast<ast::ptr_type *>(operand_type);
                assert(ptr_type != nullptr);
                result_type = ptr_type->pointed_to_type();
            }
            break;
        }

        bind_type(&unary_operation, result_type);
        visited.emplace(&unary_operation);
        unary_operation.next->accept(*this);
    }

} // namespace control_flow
