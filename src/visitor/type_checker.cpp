#include "type_checker.hpp"

#include "ast/nodes.hpp"
#include "ast/token_to_string.hpp"
#include "ast/type.hpp"

#include <memory> // make_shared
#include <sstream>

#include <llvm/ADT/Twine.h>
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>
#include <llvm/Support/Casting.h>

namespace visitor {

    type_checker::type_checker(std::string filename, llvm::LLVMContext & context,
                               global_map<std::string, ast::type_ptr> & globals)
        : filename{std::move(filename)}
        , context{context}
        , program_globals{globals} {
        instrinics.emplace("syscall", &type_checker::syscall);
    }

    void type_checker::bind_type(ast::type_ptr type, std::string identifier, bool should_export) {
        if (should_export) { program_globals.add(filename, identifier, type); }
        active_typed_identifiers.add_to_current_scope(std::move(identifier), std::move(type));
    }

    template<class... arg_t>
    void type_checker::printError(std::optional<Location> loc, const arg_t &... args) {
        std::stringstream to_print;

        if (loc.has_value()) { to_print << *loc << " : "; }

        (to_print << ... << args);

        context.emitError(to_print.str());

        found_error = true;
    }

    [[nodiscard]] ast::type_ptr type_checker::find_type_of(const std::string & identifier) const {

        for (const auto & scope : active_typed_identifiers) {
            if (auto result = scope.find(identifier); result != scope.end()) {
                return result->second;
            }
        }

        return nullptr;
    }

    void type_checker::syscall(ast::func_call_data & func_call_data) {
        // Syscalls can only take between 1 and 7 arguments.
        static constexpr auto max_syscall_args = 7U;
        if (const auto arg_count = func_call_data.args_count();
            arg_count == 0 or arg_count > max_syscall_args) {
            printError(std::nullopt, "syscalls can only take 1 to 7 arguments\nFound one with ",
                       arg_count);
        }

        for (auto i = 0U; i < func_call_data.args_count(); ++i) {
            auto & arg = func_call_data.arg(i);
            auto arg_type = get_value(arg, *this);
            if (not arg_type->is_pointer_type() and arg_type != ast::prim_type::int32) {
                printError(std::nullopt, "syscall can only take int or pointer arguments");
            }

            // The first argmuent must always be a syscall number.
            if (i == 0 and arg_type != ast::prim_type::int32) {
                printError(std::nullopt, "syscall must have an integer as its first argument");
            }
        }

        // TODO: syscalls can return pointers and 64 bit numbers
        value_getter::store_result(ast::prim_type::int32);
    }

    ast::type_ptr type_checker::evaluate_arithmetic(ast::type_ptr && lhs_type,
                                                    ast::type_ptr && rhs_type) {
        // Special case: ptr + int
        if (lhs_type->is_pointer_type() or rhs_type->is_pointer_type()) {
            if (const auto other_type = lhs_type->is_pointer_type() ? rhs_type : lhs_type;
                other_type != ast::prim_type::int32) {
                printError(std::nullopt, "Pointer types can only be added with integers");
            }

            return lhs_type->is_pointer_type() ? lhs_type : rhs_type;
        }

        if (lhs_type != rhs_type) {
            printError(std::nullopt, "Arithmetic operations can only be made within the same type");
            assert(false);
        }

        if (lhs_type == ast::prim_type::boolean or rhs_type == ast::prim_type::boolean) {
            printError(std::nullopt, "Arithmetic operations cannot be done on booleans");
            assert(false);
        }

        return lhs_type;
    }

    // NOLINTNEXTLINE
    ast::type_ptr type_checker::evaluate_comparison(ast::type_ptr && lhs_type,
                                                    ast::type_ptr && rhs_type) {

        assert(lhs_type != nullptr and rhs_type != nullptr);

        if (bool pointer_comp = (lhs_type == ast::prim_type::null or lhs_type->is_pointer_type())
                            and (rhs_type == ast::prim_type::null or rhs_type->is_pointer_type());
            not pointer_comp and lhs_type != rhs_type) {
            printError(std::nullopt, "Comparisons can only be made within the same type");
        } else if (pointer_comp) {

            // If both are null, fail.
            if (lhs_type == ast::prim_type::null and rhs_type == ast::prim_type::null) {
                printError(std::nullopt, "`null` cannot be on both sides of a binary expression");
            }

            // Check that the other pointer is nullable
            if (lhs_type == ast::prim_type::null) {
                if (auto rhs = std::dynamic_pointer_cast<ast::nullable_ptr_type>(rhs_type);
                    rhs == nullptr) {
                    printError(std::nullopt, "Only nullable pointers can be compared with `null`");
                }
            } else if (rhs_type == ast::prim_type::null) {
                if (auto lhs = std::dynamic_pointer_cast<ast::nullable_ptr_type>(lhs_type);
                    lhs == nullptr) {
                    printError(std::nullopt, "Only nullable pointers can be compared with `null`");
                }
            }

            // If either is null, no further checks are needed
            if (lhs_type == nullptr or rhs_type == nullptr) { return ast::prim_type::boolean; }

            // Check that the pointed to types are the same.

            // Shortcut for strings
            if (lhs_type == ast::prim_type::str and rhs_type == ast::prim_type::str) {
                return ast::prim_type::boolean;
            }

            auto * lhs_ptr = dynamic_cast<ast::ptr_type *>(lhs_type.get());
            auto * rhs_ptr = dynamic_cast<ast::ptr_type *>(rhs_type.get());

            assert(lhs_ptr != nullptr);
            assert(rhs_ptr != nullptr);

            if (lhs_ptr->pointed_to_type() != rhs_ptr->pointed_to_type()) {
                printError(std::nullopt, "Equality can only be made within the same type\nFound ",
                           *lhs_ptr->pointed_to_type(), " and ", *rhs_ptr->pointed_to_type());
            }
        }
        return ast::prim_type::boolean;
    }

    // NOLINTNEXTLINE
    void type_checker::visit(ast::binary_expr & binary_expr) {

        auto lhs_type = get_value(*binary_expr.lhs, *this);

        if (const auto * struct_type = dynamic_cast<ast::struct_type *>(lhs_type.get());
            struct_type != nullptr) {

            const auto * rhs = dynamic_cast<ast::user_val *>(binary_expr.rhs.get());
            if (rhs != nullptr and rhs->val_type == ast::user_val::value_type::identifier) {

                // `struct.id`

                for (auto i = 0U; i < struct_type->field_count(); ++i) {
                    if (const auto & field = struct_type->field(i); field.first == rhs->val) {
                        return store_result(field.second, binary_expr);
                    }
                }

                return printError(binary_expr.location(), "Could not find a field named ", rhs->val,
                                  " in struct type ", struct_type->user_name());
            }

            if (rhs != nullptr) {
                return printError(binary_expr.location(),
                                  "Expected an identifier to the rhs of a `.`; found ", rhs->val);
            }

            assert(false);
        }

        auto rhs_type = get_value(*binary_expr.rhs, *this);

        // If one side of a binary_expr is the `null` primitive,
        // the other side should lend it its type.
        // TODO: Check that the other side is a pointer.
        if (lhs_type == nullptr) {
            assert(rhs_type != nullptr);
            binary_expr.lhs->type = rhs_type;
        } else if (rhs_type == nullptr) {
            assert(lhs_type != nullptr);
            binary_expr.rhs->type = lhs_type;
        }

        if (binary_expr.is_shortcircuiting()) {
            const auto bool_type = ast::prim_type::boolean;
            if (lhs_type != bool_type or rhs_type != bool_type) {
                printError(binary_expr.location(), "Logical operations can only use booleans");
            }
            return store_result(bool_type, binary_expr);
        }

        if (binary_expr.is_comparison()) {
            return store_result(evaluate_comparison(std::move(lhs_type), std::move(rhs_type)),
                                binary_expr);
        }

        if (binary_expr.is_arithmetic()) {
            return store_result(evaluate_arithmetic(std::move(lhs_type), std::move(rhs_type)),
                                binary_expr);
        }

        printError(binary_expr.location(), "Unimplemented binary_expr type check for ",
                   tok_to_string(binary_expr.op));
        assert(false);
    }

    void type_checker::visit(ast::const_decl & const_decl) {

        if (find_type_of(const_decl.name_and_type.name()) != nullptr) {
            printError(const_decl.location(), "Constant ", const_decl.name_and_type.name(),
                       " has already been declared");
            assert(false);
        }

        auto expected = const_decl.name_and_type.type();
        if (expected == nullptr) {
            printError(const_decl.location(), "Type ", *const_decl.name_and_type.type(),
                       " is not known");
            assert(false);
        }

        auto actual = get_value(*const_decl.expr, *this);
        assert(actual != nullptr);

        if (expected != actual) {
            printError(const_decl.location(), "Constant ", const_decl.name_and_type.name(),
                       " is not of type ", *expected, ".\nFound type ", *actual);
        }

        bind_type(std::move(expected), const_decl.name_and_type.name(), const_decl.exported());
    }

    void type_checker::visit(ast::expr & expr) { expr.accept(*this); }

    void type_checker::visit(ast::func_call_data & func_call_data) {

        if (auto iter = instrinics.find(func_call_data.name()); iter != instrinics.end()) {
            return (this->*iter->second)(func_call_data);
        }

        auto * func_type
            = dynamic_cast<ast::function_type *>(find_type_of(func_call_data.name()).get());
        if (func_type == nullptr) {
            printError(std::nullopt, "Cannot call non-function ", func_call_data.name());
            assert(false);
        }

        if (func_type->arg_count() != func_call_data.args_count()) {
            printError(std::nullopt, func_call_data.name(), " expects ", func_type->arg_count(),
                       "arguments\nFound ", func_call_data.args_count());
            assert(false);
        }

        for (auto i = 0U; i < func_call_data.args_count(); ++i) {
            auto expected = func_type->arg(i);
            auto actual = get_value(func_call_data.arg(i), *this);
            // TODO: Wrap condition into function
            if ((actual == nullptr and not expected->is_pointer_type()) or expected != actual) {
                printError(func_call_data.arg(i).location(), "Argument ", (i + 1), " expects type ",
                           *expected, "\nFound ", *actual);
                assert(false);
            }
        }

        return value_getter::store_result(func_type->return_type());
    }

    void type_checker::visit(ast::func_call_expr & func_call_expr) {
        visit(func_call_expr.data);
        auto type = get_result();
        store_result(type, func_call_expr);
    }

    void type_checker::visit(ast::func_call_stmt & func_call_stmt) {
        visit(func_call_stmt.data);
        // NOTE: The statement must explicitly drop its type.
        drop_result();
    }

    void type_checker::visit(ast::func_decl & func_decl) {

        // add this function to the type checked ids if it does not exist

        const auto & func_name = func_decl.head.name();

        if (find_type_of(func_name) != nullptr) {
            printError(func_decl.location(), "Function ", func_name, " has already been defined");
            assert(false);
        }

        // TODO: The same logic may be present in the codegen module
        current_function_name = &func_name;
        current_return_type = func_decl.head.ret_type().get();
        if (current_return_type == nullptr) {
            printError(func_decl.location(), "Function ", func_name, " has an unknown return type ",
                       *func_decl.head.ret_type());
            assert(false);
        }

        std::vector<ast::type_ptr> arg_types;
        for (auto i = 0U; i < func_decl.head.param_count(); ++i) {
            const auto & param = func_decl.head.arg(i);
            assert(param.type() != nullptr);
            arg_types.push_back(param.type());
        }

        auto func_type
            = ast::function_type::create(func_decl.head.ret_type(), std::move(arg_types));

        bind_type(func_type, func_name, func_decl.exported());

        active_typed_identifiers.add_scope();

        // bind the parameter types
        for (auto i = 0U; i < func_decl.head.param_count(); ++i) {
            assert(func_type->arg(i) != nullptr);
            bind_type(func_type->arg(i), func_decl.head.arg(i).name());
        }

        func_decl.body->accept(*this);

        active_typed_identifiers.remove_scope();
    }

    void type_checker::visit(ast::if_expr & if_expr) {
        // Check that the condition type is bool
        auto cond_type = get_value(*if_expr.condition, *this);

        if (cond_type != ast::prim_type::boolean) {
            printError(if_expr.condition->location(),
                       "Conditions for ifs must be of type `bool`\nFound ", *cond_type);
        }

        // check that the then_case type is the same as the else_case

        auto then_type = get_value(*if_expr.then_case, *this);
        auto else_type = get_value(*if_expr.else_case, *this);

        // TODO: This seems wrong, as the `null` primitive is represented by `nullptr`
        assert(then_type != nullptr);
        assert(else_type != nullptr);

        if (then_type != else_type) {
            printError(
                if_expr.location(),
                "The then and else branches of an if expression must be of the same type\nFound ",
                *then_type, " and ", *else_type);
            assert(false);
        }
        store_result(then_type, if_expr);
    }

    void type_checker::visit(ast::if_stmt & if_stmt) {
        // Check that the condition type is bool
        auto cond_type = get_value(*if_stmt.condition, *this);

        if (cond_type != ast::prim_type::boolean) {
            printError(if_stmt.condition->location(),
                       "Conditions for ifs must be of type `bool`\nFound ", *cond_type);
        }

        if_stmt.true_branch->accept(*this);
        if (if_stmt.else_branch != nullptr) { if_stmt.else_branch->accept(*this); }
    }

    void type_checker::visit(ast::let_stmt & let_stmt) {

        if (find_type_of(let_stmt.name_and_type.name()) != nullptr) {
            printError(let_stmt.location(), let_stmt.name_and_type.name(),
                       " has already been bound to a type");
            assert(false);
        }

        auto val_type = get_value(*let_stmt.value, *this);

        if (const auto stated_type = let_stmt.name_and_type.type();
            stated_type != nullptr and stated_type != val_type) {
            printError(let_stmt.location(), let_stmt.name_and_type.name(),
                       " is expected to be of type ", *stated_type,
                       "\nFound initialization of type ", *val_type);
        } else if (val_type == nullptr) {
            if (stated_type == nullptr) {
                printError(let_stmt.location(), let_stmt.name_and_type.name(),
                           " does not specify a type, but has a `null` initialization.\nPlease "
                           "state a specific nullable pointer type for it.");
                assert(false);
            }

            val_type = stated_type;
        }

        bind_type(val_type, let_stmt.name_and_type.name());
    }

    void type_checker::visit(ast::node & node) { node.accept(*this); }

    void type_checker::visit(ast::return_stmt & return_stmt) {
        if (return_stmt.value == nullptr) {
            // we should be in a void function

            if (current_return_type != ast::prim_type::unit.get()) {
                printError(return_stmt.location(), "In function ", *current_function_name,
                           "\nExpected a return statement without an expression",
                           "\nFound one with an expression");
            }

            return;
        }

        // the expression needs to be the same as the return type

        if (auto val_type = get_value(*return_stmt.value, *this);
            val_type.get() != current_return_type) {
            printError(return_stmt.value->location(), "In function ", *current_function_name,
                       "\nExpected a return statement with expression of type ",
                       *current_return_type, "\nFound expression with type ", *val_type);
        }
    }

    void type_checker::visit(ast::stmt & stmt) { stmt.accept(*this); }

    void type_checker::visit(ast::stmt_sequence & stmt_sequence) {
        active_typed_identifiers.add_scope();
        for (auto & stmt : stmt_sequence.stmts) { stmt->accept(*this); }
        active_typed_identifiers.remove_scope();
    }

    void type_checker::visit(ast::struct_decl & struct_decl) {
        auto type_ptr = ast::user_type::lookup(struct_decl.name, filename);
        if (type_ptr == nullptr) {
            std::vector<ast::struct_type::field_type> fields;
            for (auto & typed_id : struct_decl.fields) {
                fields.emplace_back(typed_id.name(), typed_id.type());
            }
            type_ptr = ast::struct_type::create(std::string{struct_decl.name}, filename,
                                                std::move(fields));
        }

        if (visible_structs.find(struct_decl.name) != visible_structs.end()) {
            printError(struct_decl.location(), "Struct ", struct_decl.name,
                       " has been previously declared");
            return;
        }

        auto struct_type_ptr = std::dynamic_pointer_cast<ast::struct_type>(type_ptr);
        assert(struct_type_ptr != nullptr
               and "user_type::lookup somehow returned a non-struct user declared type");
        visible_structs.emplace(struct_decl.name, std::move(struct_type_ptr));
    }

    void type_checker::visit(ast::struct_init & struct_init) {

        auto struct_decl = std::dynamic_pointer_cast<ast::struct_type>(
            ast::user_type::lookup(struct_init.type_name, filename));

        if (struct_decl == nullptr) {
            return printError(struct_init.location(), '`', struct_init.type_name,
                              "` does not name a defined struct");
        }

        // Check each field for correct type
        for (auto i = 0U; i < struct_decl->field_count(); ++i) {
            const auto & field = struct_decl->field(i);
            auto name = field.first;

            auto iter = std::find_if(
                struct_init.initializers.begin(), struct_init.initializers.end(),
                [&name](auto & initializer) -> bool { return initializer.first == name; });

            if (iter == struct_init.initializers.end()) {
                return printError(struct_init.location(), "Missing initializer for field ", name,
                                  " of type ", struct_decl->user_name());
            }

            auto target_type = field.second;
            auto real_type = get_value(*iter->second, *this);
            if (target_type != real_type) {
                return printError(iter->second->location(), "Expected an expression of type ",
                                  *target_type, " for field ", name, "; Found one of ", *real_type);
            }
        }

        return store_result(std::move(struct_decl), struct_init);
    }

    void type_checker::visit(ast::top_level & top_level) { top_level.accept(*this); }

    void type_checker::visit(ast::top_level_sequence & top_level_sequence) {

        if (not top_level_sequence.imports.empty()) {
            for (auto & [filename, imports] : top_level_sequence.imports) {
                for (auto & id : imports) {
                    auto import_type = program_globals.lookup(filename, id);
                    if (import_type == nullptr) {
                        printError(top_level_sequence.location(), "File ", filename,
                                   " is expected to export ", id, "\nNo such export found");
                        assert(false);
                    }
                    bind_type(import_type, id);
                }
            }
        }

        for (auto & item : top_level_sequence.items) {
            assert(item != nullptr);
            item->accept(*this);
        }
    }

    void type_checker::visit(ast::typed_identifier & typed_identifier) {
        printError(typed_identifier.location(), "Internal compiler error: should not be here");
        assert(false);
    }

    void type_checker::visit(ast::unary_expr & unary_expr) {
        auto type = get_value(*unary_expr.expr, *this);
        switch (unary_expr.op) {
        case ast::unary_expr::operand::bool_not:
            if (type != ast::prim_type::boolean) {
                printError(unary_expr.location(), "Boolean not expects a ",
                           *ast::prim_type::boolean, " as its argument.\nFound ", *type);
                return store_result(ast::prim_type::boolean, unary_expr);
            }
            break;
        case ast::unary_expr::operand::deref:
            if (auto ptr_type = std::dynamic_pointer_cast<ast::nonnullable_ptr_type>(type);
                ptr_type != nullptr) {
                type = ptr_type->pointed_to_type();
            } else if (type == ast::prim_type::str) {
                // TODO: This branch may be removed later
                type = ast::prim_type::character;
            } else {
                printError(unary_expr.location(),
                           "Dereference expects a nonnull pointer as its argument.\nFound ", *type);
                assert(false);
            }
            break;
        case ast::unary_expr::operand::negate:
            if (type != ast::prim_type::int32 and type != ast::prim_type::float32) {
                printError(unary_expr.location(), "Negation expects either a ",
                           *ast::prim_type::int32, " or a ", *ast::prim_type::float32,
                           " as its argument.\nFound ", *type);
                assert(false);
            }
            break;
        }
        assert(type != nullptr);
        store_result(type, unary_expr);
    }

    void type_checker::visit(ast::user_val & user_val) {

        using val_type = ast::user_val::value_type;

        switch (user_val.val_type) {
        case val_type::identifier: {
            auto type = find_type_of(user_val.val);
            if (type == nullptr) {
                printError(user_val.location(), "The identifier ", user_val.val,
                           " has not been assigned a type yet.");
                assert(false);
            }
            store_result(type, user_val);
        } break;
        case val_type::null:
            store_result(ast::prim_type::null, user_val);
            break;
        case val_type::boolean:
            store_result(ast::prim_type::boolean, user_val);
            break;
        case val_type::floating:
            store_result(ast::prim_type::float32, user_val);
            break;
        case val_type::integer:
            store_result(ast::prim_type::int32, user_val);
            break;
        case val_type::character:
            store_result(ast::prim_type::character, user_val);
            break;
        case val_type::string:
            store_result(ast::prim_type::str, user_val);
            break;
        }
    }
} // namespace visitor
