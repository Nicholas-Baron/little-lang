#include "printer.hpp"

#include "ast/nodes.hpp"
#include "parser.hpp" // token names (should not be needed here)

#include <iostream>

namespace visitor {

    namespace {
        // TODO: Test this?
        [[nodiscard]] std::string tok_to_string(int tok) {
            switch (tok) {
            default:
                return "[Internal Bison Value]";
            case T_EQ:
                return "T_EQ";
            case T_NE:
                return "T_NE";
            case T_LT:
                return "T_LT";
            case T_GT:
                return "T_GT";
            case T_LE:
                return "T_LE";
            case T_GE:
                return "T_GE";
            case T_LPAREN:
                return "T_LPAREN";
            case T_RPAREN:
                return "T_RPAREN";
            case T_LBRACE:
                return "T_LBRACE";
            case T_RBRACE:
                return "T_RBRACE";
            case T_LBRACK:
                return "T_LBRACK";
            case T_RBRACK:
                return "T_RBRACK";
            case T_PLUS:
                return "T_PLUS";
            case T_MINUS:
                return "T_MINUS";
            case T_DIV:
                return "T_DIV";
            case T_MULT:
                return "T_MULT";
            case T_MOD:
                return "T_MOD";
            case T_COMMA:
                return "T_COMMA";
            case T_IS:
                return "T_IS";
            case T_SEMI:
                return "T_SEMI";
            case T_DOT:
                return "T_DOT";
            case T_RET:
                return "T_RET";
            case T_IF:
                return "T_IF";
            case T_ELSE:
                return "T_ELSE";
            case T_LET:
                return "T_LET";
            case T_CONST:
                return "T_CONST";
            case T_AND:
                return "T_AND";
            case T_OR:
                return "T_OR";
            case T_NOT:
                return "T_NOT";
            case T_ASSIGN:
                return "T_ASSIGN";
            case T_PROC:
                return "T_PROC";
            case T_IDENT:
                return "T_IDENT";
            case T_INT:
                return "T_INT";
            case T_CHAR:
                return "T_CHAR";
            case T_BOOL:
                return "T_BOOL";
            case T_STRING:
                return "T_STRING";
            case T_FLOAT:
                return "T_FLOAT";
            case T_PRIM_TYPE:
                return "T_PRIM_TYPE";
            }
        }
    } // namespace

    printer::printer(const std::string & name) { std::cout << "Module " << name << std::endl; }

    void printer::visit(ast::binary_expr & binary_expr) {

        binary_expr.lhs->accept(*this);
        std::cout << tok_to_string(binary_expr.tok) << '\n';
        binary_expr.rhs->accept(*this);
    }

    void printer::visit(ast::const_decl & const_decl) {
        std::cout << "const_decl" << std::endl;
        std::cout << const_decl.name_and_type.name() << " : " << const_decl.name_and_type.type()
                  << std::endl;
        const_decl.expr->accept(*this);
    }

    void printer::visit(ast::expr & expr) { expr.accept(*this); }

    void printer::visit(ast::func_call_data & func_call_data) {
        std::cout << "func_call_data" << std::endl;
        std::cout << func_call_data.name() << std::endl;
        for (auto i = 0U; i < func_call_data.args_count(); ++i) {
            func_call_data.arg(i)->accept(*this);
        }
    }

    void printer::visit(ast::func_call_expr & func_call_expr) { visit(func_call_expr.data); }

    void printer::visit(ast::func_call_stmt & func_call_stmt) { visit(func_call_stmt.data); }

    void printer::visit(ast::func_decl & func_decl) {
        std::cout << "func_decl" << std::endl;
        // TODO: Add accept to some other classes
        visit(func_decl.head);
        func_decl.body->accept(*this);
    }

    void printer::visit(ast::func_header & func_header) {
        std::cout << "func_header" << std::endl;

        std::cout << func_header.name() << " : " << func_header.ret_type() << std::endl;
        for (auto i = 0U; i < func_header.param_count(); ++i) {
            const auto & typed_identifier = func_header.arg(i);
            std::cout << i << " : " << typed_identifier.name() << " : " << typed_identifier.type()
                      << std::endl;
        }
    }

    void printer::visit(ast::if_stmt & if_stmt) {
        std::cout << "if_stmt" << std::endl;
        if_stmt.condition->accept(*this);
        if_stmt.true_branch->accept(*this);
        if (if_stmt.else_branch != nullptr) { if_stmt.else_branch->accept(*this); }
    }

    void printer::visit(ast::let_stmt & let_stmt) {
        std::cout << "let_stmt" << std::endl;
        std::cout << let_stmt.name_and_type.name() << " : " << let_stmt.name_and_type.type()
                  << std::endl;
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
        std::cout << tok_to_string(unary_expr.tok) << std::endl;
        unary_expr.expr->accept(*this);
    }

    void printer::visit(ast::user_val & user_val) { std::cout << user_val.val << std::endl; }
} // namespace visitor
