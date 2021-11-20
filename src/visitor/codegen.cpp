#include "codegen.hpp"

#include "ast/nodes.hpp"
#include "emit_asm.hpp"
#include "parser.hpp" // token names (should not be needed here)
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Verifier.h>

#include <iostream>
#include <sstream>

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

    codegen::codegen(const std::string & name)
        : context{std::make_unique<llvm::LLVMContext>()}
        , ir_module{std::make_unique<llvm::Module>(name, *context)}
        , ir_builder{std::make_unique<llvm::IRBuilder<>>(*context)}
        , types{{"int", llvm::Type::getInt32Ty(*context)},
                {"float", llvm::Type::getFloatTy(*context)},
                {"proc", llvm::Type::getVoidTy(*context)},
                {"bool", llvm::Type::getInt1Ty(*context)},
                {"char", llvm::Type::getInt8Ty(*context)}}
        , active_values{{}} {
        ir_module->setTargetTriple(init_llvm_targets());
    }

    void codegen::dump() const {
        // TODO: move away from std::cout?
        std::string to_print;
        {
            llvm::raw_string_ostream stream(to_print);
            ir_module->print(stream, nullptr);
        }

        std::cout << to_print << std::endl;
    }

    llvm::Type * codegen::find_type(const std::string & name, std::optional<Location> loc) {

        const auto iter = types.find(name);
        if (iter != types.end()) { return iter->second; }
        printError(name + " is an unknown type", loc);
        return nullptr;
    }

    void codegen::verify_module() const { llvm::verifyModule(*ir_module, &llvm::errs()); }

    void codegen::printError(const std::string & name, std::optional<Location> loc) {
        if (loc == std::nullopt) {
            context->emitError(name);
        } else {
            std::stringstream to_print;
            to_print << *loc << " : " << name;
            context->emitError(to_print.str());
        }
    }

    void codegen::visit(ast::binary_expr & binary_expr) {

        binary_expr.lhs->accept(*this);
        std::cout << tok_to_string(binary_expr.tok) << '\n';
        binary_expr.rhs->accept(*this);
    }

    void codegen::visit(ast::const_decl & const_decl) {
        std::cout << "const_decl" << std::endl;
        std::cout << const_decl.name_and_type.name() << " : " << const_decl.name_and_type.type()
                  << std::endl;
        const_decl.expr->accept(*this);
    }

    void codegen::visit(ast::expr & expr) { expr.accept(*this); }

    void codegen::visit(ast::func_call_data & func_call_data) {
        std::cout << "func_call_data" << std::endl;
        std::cout << func_call_data.name() << std::endl;
        for (auto i = 0U; i < func_call_data.args_count(); ++i) {
            func_call_data.arg(i)->accept(*this);
        }
    }

    void codegen::visit(ast::func_call_expr & func_call_expr) {
        // TODO: Add accept to some other classes
        visit(func_call_expr.data);
    }

    void codegen::visit(ast::func_call_stmt & func_call_stmt) { visit(func_call_stmt.data); }

    void codegen::visit(ast::func_decl & func_decl) {

        std::vector<llvm::Type *> param_types;
        auto param_count = func_decl.head.param_count();
        param_types.reserve(func_decl.head.param_count());

        for (auto i = 0U; i < param_count; ++i) {

            auto param = func_decl.head.arg(i);
            param_types.push_back(find_type(param.type(), param.location()));
        }

        auto * func_type = llvm::FunctionType::get(
            find_type(func_decl.head.ret_type(), func_decl.location()), param_types, false);

        auto * func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage,
                                             func_decl.head.name(), ir_module.get());

        // add the function to the current scope
        active_values.back().emplace(func_decl.head.name(), func);

        // enter the function
        active_values.emplace_back();
        for (auto i = 0U; i < param_count; ++i) {
            const auto & name = func_decl.head.arg(i).name();
            auto * arg = func->getArg(i);
            arg->setName(name);
            active_values.back().emplace(name, arg);
        }

        auto * block = llvm::BasicBlock::Create(*context, "", func);
        ir_builder->SetInsertPoint(block);

        func_decl.body->accept(*this);

        // leave the function
        active_values.pop_back();
    }

    void codegen::visit(ast::func_header & func_header) {
        std::cout << "func_header" << std::endl;

        std::cout << func_header.name() << " : " << func_header.ret_type() << std::endl;
        for (auto i = 0U; i < func_header.param_count(); ++i) {
            const auto & typed_identifier = func_header.arg(i);
            std::cout << i << " : " << typed_identifier.name() << " : " << typed_identifier.type()
                      << std::endl;
        }
    }

    void codegen::visit(ast::if_stmt & if_stmt) {

        // Evaluate the condition
        llvm::Value * condition = get_value(*if_stmt.condition, *this);

        auto * start_block = ir_builder->GetInsertBlock();
        auto * current_function = start_block->getParent();

		// Generate the true branch
        auto * then_block = llvm::BasicBlock::Create(*context, "", current_function);
        ir_builder->SetInsertPoint(then_block);
        if_stmt.true_branch->accept(*this);

        auto terminated
            = [](const llvm::BasicBlock * block) -> bool { return block->back().isTerminator(); };

        if (if_stmt.else_branch == nullptr) {

			// Short cut to just merging
            auto * merge_block = llvm::BasicBlock::Create(*context, "", current_function);

            if (not terminated(start_block)) {
                ir_builder->SetInsertPoint(start_block);
                ir_builder->CreateCondBr(condition, then_block, merge_block);
            }
            ir_builder->SetInsertPoint(merge_block);

            return;
        }

		// Generate the else block
        auto * else_block = llvm::BasicBlock::Create(*context, "", current_function);
        ir_builder->SetInsertPoint(else_block);
        if_stmt.else_branch->accept(*this);

        ir_builder->SetInsertPoint(start_block);
        ir_builder->CreateCondBr(condition, then_block, else_block);

		// Ensure termination
        if (not terminated(else_block) and not terminated(then_block)) {
            auto * merge_block = llvm::BasicBlock::Create(*context, "", current_function);
            if (not terminated(then_block)) {
                ir_builder->SetInsertPoint(then_block);
                ir_builder->CreateBr(merge_block);
            }

            ir_builder->SetInsertPoint(merge_block);
        }
    }

    void codegen::visit(ast::let_stmt & let_stmt) {
        std::cout << "let_stmt" << std::endl;
        std::cout << let_stmt.name_and_type.name() << " : " << let_stmt.name_and_type.type()
                  << std::endl;
        let_stmt.value->accept(*this);
    }

    void codegen::visit(ast::node & node) { node.accept(*this); }

    void codegen::visit(ast::return_stmt & return_stmt) {
        std::cout << "return_stmt" << std::endl;
        if (return_stmt.value != nullptr) { return_stmt.value->accept(*this); }
    }

    void codegen::visit(ast::stmt & stmt) { stmt.accept(*this); }

    void codegen::visit(ast::stmt_sequence & stmt_sequence) {
        std::cout << "stmt_sequence" << std::endl;
        for (auto & stmt : stmt_sequence.stmts) { stmt->accept(*this); }
    }

    void codegen::visit(ast::top_level & top_level) { top_level.accept(*this); }

    void codegen::visit(ast::top_level_sequence & top_level_sequence) {
        for (auto & item : top_level_sequence.items) {
            assert(item != nullptr);
            item->accept(*this);
        }
    }

    void codegen::visit(ast::typed_identifier & /*typed_identifier*/) {
        std::cout << "How did I get here?" << std::endl;
        assert(false);
    }

    void codegen::visit(ast::unary_expr & unary_expr) {
        std::cout << "unary_expr" << std::endl;
        std::cout << tok_to_string(unary_expr.tok) << std::endl;
        unary_expr.expr->accept(*this);
    }

    void codegen::visit(ast::user_val & user_val) { std::cout << user_val.val << std::endl; }
} // namespace visitor
