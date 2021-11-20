#include "codegen.hpp"

#include <iostream>

namespace visitor {
    codegen::codegen(const std::string & name)
        : context{std::make_unique<llvm::LLVMContext>()}
        , ir_module{std::make_unique<llvm::Module>(name, *context)}
        , ir_builder{std::make_unique<llvm::IRBuilder<>>(*context)} {}

    void codegen::visit(ast::binary_expr & binary_expr) { std::cout << "binary_expr" << std::endl; }

    void codegen::visit(ast::const_decl & const_decl) { std::cout << "const_decl" << std::endl; }

    void codegen::visit(ast::expr & expr) { std::cout << "expr" << std::endl; }

    void codegen::visit(ast::func_call_data & func_call_data) {
        std::cout << "func_call_data" << std::endl;
    }

    void codegen::visit(ast::func_call_expr & func_call_expr) {
        std::cout << "func_call_expr" << std::endl;
    }

    void codegen::visit(ast::func_call_stmt & func_call_stmt) {
        std::cout << "func_call_stmt" << std::endl;
    }

    void codegen::visit(ast::func_decl & func_decl) { std::cout << "func_decl" << std::endl; }

    void codegen::visit(ast::func_header & func_header) { std::cout << "func_header" << std::endl; }

    void codegen::visit(ast::if_stmt & if_stmt) { std::cout << "if_stmt" << std::endl; }

    void codegen::visit(ast::let_stmt & let_stmt) { std::cout << "let_stmt" << std::endl; }

    void codegen::visit(ast::node & node) { std::cout << "node" << std::endl; }

    void codegen::visit(ast::return_stmt & return_stmt) { std::cout << "return_stmt" << std::endl; }

    void codegen::visit(ast::stmt & stmt) { std::cout << "stmt" << std::endl; }

    void codegen::visit(ast::stmt_sequence & stmt_sequence) {
        std::cout << "stmt_sequence" << std::endl;
    }

    void codegen::visit(ast::top_level & top_level) { std::cout << "top_level" << std::endl; }

    void codegen::visit(ast::top_level_sequence & top_level_sequence) {
        std::cout << "top_level_sequence" << std::endl;
    }

    void codegen::visit(ast::typed_identifier & typed_identifier) {
        std::cout << "typed_identifier" << std::endl;
    }

    void codegen::visit(ast::unary_expr & unary_expr) { std::cout << "unary_expr" << std::endl; }

    void codegen::visit(ast::user_val & user_val) { std::cout << "user_val" << std::endl; }
} // namespace visitor
