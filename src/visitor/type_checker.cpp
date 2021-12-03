#include "type_checker.hpp"

#include "ast/nodes.hpp"
#include "parser.hpp" // please remove
#include "token_to_string.hpp"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

#include <iostream>
#include <memory>

namespace visitor {

    type_checker::type_checker()
        : context{std::make_unique<llvm::LLVMContext>()} {
        auto & global_scope = active_typed_identifiers.emplace_back();

        global_scope.emplace("int", llvm::Type::getInt32Ty(*context));
        global_scope.emplace("float", llvm::Type::getFloatTy(*context));
        global_scope.emplace("proc", llvm::Type::getVoidTy(*context));
        global_scope.emplace("bool", llvm::Type::getInt1Ty(*context));
        global_scope.emplace("char", llvm::Type::getInt8Ty(*context));

        // TODO: Move to a Rust style 2 ptr string instead of a C style null-terminated string
        global_scope.emplace("string", llvm::Type::getInt8PtrTy(*context));
    }

    void type_checker::bind_type(llvm::Type * type, std::string identifier) {
        active_typed_identifiers.back().emplace(std::move(identifier), type);
    }

    [[nodiscard]] llvm::Type * type_checker::find_type_of(const std::string & identifier) const {

        for (auto iter = active_typed_identifiers.rbegin(); iter != active_typed_identifiers.rend();
             ++iter) {
            if (auto result = iter->find(identifier); result != iter->end()) {
                return result->second;
            }
        }

        return nullptr;
    }

    void type_checker::visit(ast::binary_expr & binary_expr) {

        auto * lhs_type = get_value(*binary_expr.lhs, *this);
        auto * rhs_type = get_value(*binary_expr.rhs, *this);

        switch (binary_expr.tok) {
        case T_OR:
        case T_AND:
            if (auto * bool_type = find_type_of("bool");
                lhs_type != bool_type or rhs_type != bool_type) {
                std::cout << "Logical operations can only use booleans" << std::endl;
                assert(false);
            }
            [[fallthrough]];
        case T_EQ:
        case T_NE:
        case T_GT:
        case T_GE:
        case T_LT:
        case T_LE:
            if (lhs_type != rhs_type) {
                std::cout << "Comparisons can only be made within the same type" << std::endl;
                assert(false);
            }
            store_result(find_type_of("bool"));
            break;
        case T_PLUS:
        case T_MINUS:
        case T_MULT:
            if (lhs_type != rhs_type) {
                std::cout << "Arithmetic operations can only be made within the same type"
                          << std::endl;
                assert(false);
            }
            store_result(lhs_type);
            break;
        default:
            std::cout << "Unimplemented type check for " << tok_to_string(binary_expr.tok)
                      << std::endl;
            assert(false);
        }
    }

    void type_checker::visit(ast::const_decl & const_decl) {

        if (find_type_of(const_decl.name_and_type.name()) != nullptr) {
            std::cout << "Constant " << const_decl.name_and_type.name()
                      << " has already been declared" << std::endl;
            assert(false);
        }

        auto * expected = find_type_of(const_decl.name_and_type.type());
        if (expected == nullptr) {
            std::cout << "Type " << const_decl.name_and_type.type() << " is not known" << std::endl;
            assert(false);
        }

        auto * actual = get_value(*const_decl.expr, *this);
        if (expected != actual) {
            std::cout << "Constant " << const_decl.name_and_type.name() << " is not of type "
                      << const_decl.name_and_type.type() << std::endl;
            assert(false);
        }

        bind_type(actual, const_decl.name_and_type.name());
    }

    void type_checker::visit(ast::expr & expr) { expr.accept(*this); }

    void type_checker::visit(ast::func_call_data & func_call_data) {

        auto * func_type
            = llvm::dyn_cast_or_null<llvm::FunctionType>(find_type_of(func_call_data.name()));
        if (func_type == nullptr or not func_type->isFunctionTy()) {
            std::cout << "Cannot call non-function " << func_call_data.name() << std::endl;
            assert(false);
        }

        if (func_type->getNumParams() != func_call_data.args_count()) {
            std::cout << func_call_data.name()
                      << " was called with an incorrect number of arguments" << std::endl;
            assert(false);
        }

        for (auto i = 0U; i < func_call_data.args_count(); ++i) {
            auto * expected = func_type->getParamType(i);
            if (expected != get_value(*func_call_data.arg(i), *this)) {
                std::cout << "Argument #" << (i + 1) << " was of an incorrect type" << std::endl;
                assert(false);
            }
        }

        store_result(func_type->getReturnType());
    }

    void type_checker::visit(ast::func_call_expr & func_call_expr) { visit(func_call_expr.data); }

    void type_checker::visit(ast::func_call_stmt & func_call_stmt) { visit(func_call_stmt.data); }

    void type_checker::visit(ast::func_decl & func_decl) {

        // add this function to the type checked ids if it does not exist

        const auto & func_name = func_decl.head.name();

        if (find_type_of(func_name) != nullptr) {
            std::cout << "Function " << func_name << " has already been defined?" << std::endl;
            assert(false);
        }

        // TODO: The same logic may be present in the codegen module
        auto * ret_type = find_type_of(func_decl.head.ret_type());
        if (ret_type == nullptr) {
            std::cout << func_decl.head.ret_type() << " is not a known type" << std::endl;
            assert(false);
        }

        current_return_type = ret_type;

        std::vector<llvm::Type *> args;
        {
            for (auto i = 0U; i < func_decl.head.param_count(); ++i) {
                const auto & param = func_decl.head.arg(i);

                auto * arg_type = find_type_of(param.type());
                if (arg_type == nullptr) {
                    std::cout << param.type() << " is not a known type" << std::endl;
                    assert(false);
                }
                args.emplace_back(arg_type);
            }
        }

        auto * func_type = llvm::FunctionType::get(ret_type, args, false);

        bind_type(func_type, func_name);

        active_typed_identifiers.emplace_back();

        // bind the parameter types
        for (auto i = 0U; i < func_decl.head.param_count(); ++i) {
            bind_type(args[i], func_decl.head.arg(i).name());
        }

        func_decl.body->accept(*this);

        active_typed_identifiers.pop_back();
    }

    void type_checker::visit(ast::if_stmt & if_stmt) {
        // Check that the condition type is bool
        auto * cond_type = get_value(*if_stmt.condition, *this);

        // TODO: improve this check
        if (cond_type != find_type_of("bool")) {
            std::cout << "Conditions for ifs must be of type `bool`" << std::endl;
            assert(false);
        }

        if_stmt.true_branch->accept(*this);
        if (if_stmt.else_branch != nullptr) { if_stmt.else_branch->accept(*this); }
    }

    void type_checker::visit(ast::let_stmt & let_stmt) {

        if (find_type_of(let_stmt.name_and_type.name()) != nullptr) {
            std::cout << let_stmt.name_and_type.name() << " has already been bound to a type"
                      << std::endl;
            assert(false);
        }

        auto * val_type = get_value(*let_stmt.value, *this);

        if (auto stated_type = let_stmt.name_and_type.type();
            not stated_type.empty() and stated_type != "auto") {
            if (auto * found_type = find_type_of(stated_type); found_type != val_type) {
                std::cout << stated_type << " is not the type of the initialization of "
                          << let_stmt.name_and_type.name() << std::endl;
                assert(false);
            }
        }

        bind_type(val_type, let_stmt.name_and_type.name());
    }

    void type_checker::visit(ast::node & node) { node.accept(*this); }

    void type_checker::visit(ast::return_stmt & return_stmt) {
        if (return_stmt.value == nullptr) {
            // we should be in a void function

            if (not current_return_type->isVoidTy()) {
                std::cout << "Return should have an expression in a 'non-void function'"
                          << std::endl;
                assert(false);
            }

            return;
        }

        // the expression needs to be the same as the return type

        auto * val_type = get_value(*return_stmt.value, *this);

        if (val_type != current_return_type) {
            std::cout << "Return statement with wrong type of expression found" << std::endl;
            assert(false);
        }
    }

    void type_checker::visit(ast::stmt & stmt) { stmt.accept(*this); }

    void type_checker::visit(ast::stmt_sequence & stmt_sequence) {
        active_typed_identifiers.emplace_back();
        for (auto & stmt : stmt_sequence.stmts) { stmt->accept(*this); }
        active_typed_identifiers.pop_back();
    }

    void type_checker::visit(ast::top_level & top_level) { top_level.accept(*this); }

    void type_checker::visit(ast::top_level_sequence & top_level_sequence) {
        for (auto & item : top_level_sequence.items) {
            assert(item != nullptr);
            item->accept(*this);
        }
    }

    void type_checker::visit(ast::typed_identifier & /*typed_identifier*/) {
        std::cout << "How did I get here?" << std::endl;
        assert(false);
    }

    void type_checker::visit(ast::unary_expr & unary_expr) {
        std::cout << "unary_expr" << std::endl;
        std::cout << tok_to_string(unary_expr.tok) << std::endl;
        unary_expr.expr->accept(*this);
    }

    void type_checker::visit(ast::user_val & user_val) {

        using val_type = ast::user_val::value_type;

        switch (user_val.type) {
        case val_type::identifier: {
            auto * type = find_type_of(user_val.val);
            if (type == nullptr) {
                std::cout << "Identifier " << user_val.val << " has not been typed yet"
                          << std::endl;
                assert(false);
            }
            store_result(type);
        } break;
        case val_type::boolean:
            store_result(find_type_of("bool"));
            break;
        case val_type::floating:
            store_result(find_type_of("float"));
            break;
        case val_type::integer:
            store_result(find_type_of("int"));
            break;
        case val_type::character:
            store_result(find_type_of("char"));
            break;
        case val_type::string:
            store_result(find_type_of("string"));
            break;
        }
    }
} // namespace visitor
