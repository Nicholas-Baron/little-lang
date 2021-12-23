#include "printer.hpp"

#include "ast/nodes.hpp"
#include "token_to_string.hpp"

#include <iostream>

namespace visitor {

    printer::printer(const std::string & name) { std::cout << "Module " << name << std::endl; }

    void printer::visit(ast::binary_expr & binary_expr) {

        binary_expr.lhs->accept(*this);
        std::cout << tok_to_string(binary_expr.op) << '\n';
        binary_expr.rhs->accept(*this);
    }

    void printer::visit(ast::const_decl & const_decl) {
        std::cout << "const_decl" << std::endl;
        std::cout << const_decl.name_and_type.name() << " : "
                  << const_decl.name_and_type.type().base_type() << std::endl;
        const_decl.expr->accept(*this);
    }

    void printer::visit(ast::expr & expr) { expr.accept(*this); }

    void printer::visit(ast::func_call_data & func_call_data) {
        std::cout << "func_call_data" << std::endl;
        std::cout << func_call_data.name() << std::endl;
        for (auto i = 0U; i < func_call_data.args_count(); ++i) {
            func_call_data.arg(i).accept(*this);
        }
    }

    void printer::visit(ast::func_call_expr & func_call_expr) { visit(func_call_expr.data); }

    void printer::visit(ast::func_call_stmt & func_call_stmt) { visit(func_call_stmt.data); }

    void printer::visit(ast::func_decl & func_decl) {
        std::cout << "func_decl" << std::endl;
        // TODO: Add accept to some other classes
        std::cout << func_decl.head.name() << " : " << func_decl.head.ret_type().base_type()
                  << std::endl;
        for (auto i = 0U; i < func_decl.head.param_count(); ++i) {
            const auto & typed_identifier = func_decl.head.arg(i);
            std::cout << i << " : " << typed_identifier.name() << " : "
                      << typed_identifier.type().base_type() << std::endl;
        }
        func_decl.body->accept(*this);
    }

    void printer::visit(ast::if_expr & if_expr) {
        std::cout << "if_expr" << std::endl;
        if_expr.condition->accept(*this);
        if_expr.then_case->accept(*this);
        if_expr.else_case->accept(*this);
    }

    void printer::visit(ast::if_stmt & if_stmt) {
        std::cout << "if_stmt" << std::endl;
        if_stmt.condition->accept(*this);
        if_stmt.true_branch->accept(*this);
        if (if_stmt.else_branch != nullptr) { if_stmt.else_branch->accept(*this); }
    }

    void printer::visit(ast::let_stmt & let_stmt) {
        std::cout << "let_stmt" << std::endl;
        std::cout << let_stmt.name_and_type.name() << " : "
                  << let_stmt.name_and_type.type().base_type() << std::endl;
        let_stmt.value->accept(*this);
    }

    void printer::visit(ast::node & node) { node.accept(*this); }

    void printer::visit(ast::return_stmt & return_stmt) {
        std::cout << "return_stmt" << std::endl;
        if (return_stmt.value != nullptr) { return_stmt.value->accept(*this); }
    }

    void printer::visit(ast::stmt & stmt) { stmt.accept(*this); }

    void printer::visit(ast::stmt_sequence & stmt_sequence) {
        std::cout << "stmt_sequence" << std::endl;
        for (auto & stmt : stmt_sequence.stmts) { stmt->accept(*this); }
    }

    void printer::visit(ast::top_level & top_level) { top_level.accept(*this); }

    void printer::visit(ast::top_level_sequence & top_level_sequence) {
        std::cout << "top_level_sequence" << std::endl;

        for (auto & [file, imports] : top_level_sequence.imports) {
            std::cout << file << " : ";
            for (auto & id : imports) { std::cout << id << ", "; }
            std::cout << std::endl;
        }

        for (auto & item : top_level_sequence.items) {
            assert(item != nullptr);
            visit(*item);
        }
    }

    void printer::visit(ast::typed_identifier & /*typed_identifier*/) {
        std::cout << "How did I get here?" << std::endl;
        assert(false);
    }

    void printer::visit(ast::unary_expr & unary_expr) {
        std::cout << "unary_expr" << std::endl;
        std::cout << tok_to_string(unary_expr.op) << std::endl;
        unary_expr.expr->accept(*this);
    }

    void printer::visit(ast::user_val & user_val) { std::cout << user_val.val << std::endl; }
} // namespace visitor
