#include "type_checker.hpp"

#include "ast/nodes.hpp"
#include "token_to_string.hpp"
#include <llvm/IR/DerivedTypes.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

#include <iostream>
#include <memory>

namespace visitor {

    type_checker::type_checker(std::string filename, llvm::LLVMContext * context,
                               global_map<llvm::Type *> * imports)
        : filename{std::move(filename)}
        , context{context}
        , program_globals{imports} {
        auto & global_scope = active_typed_identifiers.emplace_back();

        global_scope.emplace("int", llvm::Type::getInt32Ty(*context));
        global_scope.emplace("float", llvm::Type::getFloatTy(*context));
        global_scope.emplace("unit", llvm::Type::getVoidTy(*context));
        global_scope.emplace("bool", llvm::Type::getInt1Ty(*context));
        global_scope.emplace("char", llvm::Type::getInt8Ty(*context));

        // TODO: Move to a Rust style 2 ptr string instead of a C style null-terminated string
        global_scope.emplace("string", llvm::Type::getInt8PtrTy(*context));

        instrinics.emplace("syscall", &type_checker::syscall);
    }

    void type_checker::bind_type(llvm::Type * type, std::string identifier, bool should_export) {
        if (should_export and program_globals != nullptr) {
            program_globals->add(filename, identifier, type);
        }
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

    void type_checker::syscall(ast::func_call_data & func_call_data) {
        // Syscalls can only take between 1 and 7 arguments.
        static constexpr auto max_syscall_args = 7U;
        if (const auto arg_count = func_call_data.args_count();
            arg_count == 0 or arg_count > max_syscall_args) {
            std::cout << "syscalls can only take 1 to 7 arguments" << std::endl;
            assert(false);
        }

        for (auto i = 0U; i < func_call_data.args_count(); ++i) {
            auto & arg = func_call_data.arg(i);
            auto * arg_type = get_value(arg, *this);
            if (not arg_type->isIntOrPtrTy()) {
                std::cout << "syscall can only take int, bool, or string arguments" << std::endl;
                assert(false);
            }

            // The first argmuent must always be a syscall number.
            if (i == 0 and not arg_type->isIntegerTy()) {
                std::cout << "syscall must have an integer as its first argument" << std::endl;
                assert(false);
            }
        }
        // TODO: syscalls can return pointers and 64 bit numbers
        store_result(find_type_of("int"));
    }

    void type_checker::visit(ast::binary_expr & binary_expr) {

        auto * lhs_type = get_value(*binary_expr.lhs, *this);
        auto * rhs_type = get_value(*binary_expr.rhs, *this);

        using operand = ast::binary_expr::operand;
        switch (binary_expr.op) {
        case operand::bool_or:
        case operand::bool_and:
            if (auto * bool_type = find_type_of("bool");
                lhs_type != bool_type or rhs_type != bool_type) {
                std::cout << "Logical operations can only use booleans" << std::endl;
                assert(false);
            }
            [[fallthrough]];
        case operand::eq:
        case operand::ne:
        case operand::gt:
        case operand::ge:
        case operand::lt:
        case operand::le:
            if (lhs_type != rhs_type) {
                std::cout << "Comparisons can only be made within the same type" << std::endl;
                assert(false);
            }
            store_result(find_type_of("bool"));
            break;
        case operand::add:
        case operand::sub:
        case operand::mult:
        case operand::div:
            if (lhs_type != rhs_type) {
                std::cout << "Arithmetic operations can only be made within the same type"
                          << std::endl;
                assert(false);
            }
            store_result(lhs_type);
            break;
        default:
            std::cout << "Unimplemented type check for " << tok_to_string(binary_expr.op)
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

        bind_type(actual, const_decl.name_and_type.name(), const_decl.exported());
    }

    void type_checker::visit(ast::expr & expr) { expr.accept(*this); }

    void type_checker::visit(ast::func_call_data & func_call_data) {

        if (auto iter = instrinics.find(func_call_data.name()); iter != instrinics.end()) {
            return (this->*iter->second)(func_call_data);
        }

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
            if (expected != get_value(func_call_data.arg(i), *this)) {
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

        bind_type(func_type, func_name, func_decl.exported());

        active_typed_identifiers.emplace_back();

        // bind the parameter types
        for (auto i = 0U; i < func_decl.head.param_count(); ++i) {
            bind_type(args[i], func_decl.head.arg(i).name());
        }

        func_decl.body->accept(*this);

        active_typed_identifiers.pop_back();
    }

    void type_checker::visit(ast::if_expr & if_expr) {
        // Check that the condition type is bool
        auto * cond_type = get_value(*if_expr.condition, *this);

        // TODO: improve this check
        if (cond_type != find_type_of("bool")) {
            std::cout << "Conditions for ifs must be of type `bool`" << std::endl;
            assert(false);
        }

        // check that the then_case type is the same as the else_case

        auto * then_type = get_value(*if_expr.then_case, *this);
        auto * else_type = get_value(*if_expr.else_case, *this);
        assert(then_type != nullptr);
        assert(else_type != nullptr);

        if (then_type != else_type) {
            std::cout << "The then and else branches of an if expression must be of the same type"
                      << std::endl;
            assert(false);
        }
        store_result(then_type);
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

        if (not top_level_sequence.imports.empty()) {
            if (this->program_globals == nullptr) {
                std::cerr << "No import map was given" << std::endl;
                assert(false);
            }
            for (auto & [filename, imports] : top_level_sequence.imports) {
                for (auto & id : imports) {
                    auto * import_type = this->program_globals->lookup(filename, id);
                    if (import_type == nullptr) {
                        std::cout << "File " << filename << " does not export " << id << std::endl;
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

    void type_checker::visit(ast::typed_identifier & /*typed_identifier*/) {
        std::cout << "How did I get here?" << std::endl;
        assert(false);
    }

    void type_checker::visit(ast::unary_expr & unary_expr) {
        std::cout << "unary_expr" << std::endl;
        std::cout << tok_to_string(unary_expr.op) << std::endl;
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
