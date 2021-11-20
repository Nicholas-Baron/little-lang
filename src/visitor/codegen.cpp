#include "codegen.hpp"

#include "ast/nodes.hpp"
#include "emit_asm.hpp"
#include "parser.hpp" // token names (should not be needed here)
#include <llvm/IR/BasicBlock.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/InstrTypes.h>
#include <llvm/IR/Instruction.h>
#include <llvm/IR/Verifier.h>
#include <llvm/Support/raw_ostream.h>

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

        const std::map<std::string, bool> valid_bools{{"true", true},   {"True", true},
                                                      {"TRUE", true},   {"false", false},
                                                      {"False", false}, {"FALSE", false}};
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

    llvm::Value * codegen::find_alive_value(const std::string & name) const {
        // Walk backwards thru scopes
        for (auto scope = active_values.rbegin(); scope != active_values.rend(); ++scope) {
            if (auto iter = scope->find(name); iter != scope->end()) { return iter->second; }
        }
        printError("Could not find value " + name);
        return nullptr;
    }

    llvm::Type * codegen::find_type(const std::string & name, std::optional<Location> loc) {

        const auto iter = types.find(name);
        if (iter != types.end()) { return iter->second; }
        printError(name + " is an unknown type", loc);
        return nullptr;
    }

    void codegen::verify_module() const { llvm::verifyModule(*ir_module, &llvm::errs()); }

    void codegen::printError(const std::string & name, std::optional<Location> loc) const {
        if (loc == std::nullopt) {
            context->emitError(name);
        } else {
            std::stringstream to_print;
            to_print << *loc << " : " << name;
            context->emitError(to_print.str());
        }
    }

    void codegen::visit(ast::binary_expr & binary_expr) {

        auto * lhs_value = get_value(*binary_expr.lhs, *this);
        if (binary_expr.is_shortcircuiting()) {
            // TODO: Too many arrows
            auto * lhs_block = ir_builder->GetInsertBlock();
            auto * current_function = lhs_block->getParent();
            auto * rhs_block = llvm::BasicBlock::Create(*context, "rhs", current_function);

            auto * merge_block = llvm::BasicBlock::Create(*context, "merge", current_function);

            // add a check to short circuit
            // short on false if anding, short on true if oring

            assert(binary_expr.tok == T_AND or binary_expr.tok == T_OR);

            // if (true && rhs) -> should eval rhs
            auto * on_true = binary_expr.tok == T_AND ? rhs_block : merge_block;

            // if (false || rhs) -> should eval rhs
            auto * on_false = binary_expr.tok == T_OR ? rhs_block : merge_block;
            assert(on_true != on_false);

            ir_builder->CreateCondBr(lhs_value, on_true, on_false);

            ir_builder->SetInsertPoint(rhs_block);
            auto * rhs_value = get_value(*binary_expr.rhs, *this);
            ir_builder->CreateBr(merge_block);

            ir_builder->SetInsertPoint(merge_block);
            auto * phi = ir_builder->CreatePHI(lhs_value->getType(), 2);
            phi->addIncoming(lhs_value, lhs_block);
            phi->addIncoming(rhs_value, rhs_block);

            store_result(phi);
            return;
        }

        // We will generate here, as every expression after will need the rhs
        auto * rhs_value = get_value(*binary_expr.rhs, *this);

        const bool is_constant
            = llvm::isa<llvm::Constant>(lhs_value) and llvm::isa<llvm::Constant>(rhs_value);

        const bool is_int
            = lhs_value->getType()->isIntegerTy() and rhs_value->getType()->isIntegerTy();

        if (binary_expr.is_comparison()) {

            using predicate = llvm::CmpInst::Predicate;
            auto int_or_float = [&is_int](predicate float_pred, predicate int_pred) {
                return is_int ? int_pred : float_pred;
            };

            std::optional<predicate> p;
            switch (binary_expr.tok) {
            case T_LE:
                p = int_or_float(predicate::FCMP_OLE, predicate::ICMP_SLE);
                break;
            case T_LT:
                p = int_or_float(predicate::FCMP_OLT, predicate::ICMP_SLT);
                break;
            case T_GE:
                p = int_or_float(predicate::FCMP_OGE, predicate::ICMP_SGE);
                break;
            case T_GT:
                p = int_or_float(predicate::FCMP_OGT, predicate::ICMP_SGT);
                break;
            case T_EQ:
                p = int_or_float(predicate::FCMP_OEQ, predicate::ICMP_EQ);
                break;
            case T_NE:
                p = int_or_float(predicate::FCMP_ONE, predicate::ICMP_NE);
                break;
            default:
                printError("Comparison operator " + tok_to_string(binary_expr.tok)
                               + " is not implemented yet",
                           binary_expr.location());
                assert(false);
            }
            assert(p.has_value());
            if (is_constant) {
                auto * constant_lhs = llvm::dyn_cast<llvm::Constant>(lhs_value);
                auto * constant_rhs = llvm::dyn_cast<llvm::Constant>(rhs_value);
                store_result(llvm::ConstantExpr::getCompare(*p, constant_lhs, constant_rhs));
            } else if (is_int) {
                store_result(ir_builder->CreateICmp(*p, lhs_value, rhs_value));
            } else {
                store_result(ir_builder->CreateFCmp(*p, lhs_value, rhs_value));
            }
            return;
        }

        // all other binary expressions

        using bin_ops = llvm::Instruction::BinaryOps;
        std::optional<bin_ops> bin_op;

        // TODO: Rename or swap args
        auto int_or_float = [&is_int](bin_ops float_pred, bin_ops int_pred) {
            return is_int ? int_pred : float_pred;
        };

        switch (binary_expr.tok) {
        case T_PLUS:
            bin_op = int_or_float(bin_ops::Add, bin_ops::FAdd);
            break;
        case T_MINUS:
            bin_op = int_or_float(bin_ops::Sub, bin_ops::FSub);
            break;
        default:
            printError("Binary operator " + tok_to_string(binary_expr.tok)
                           + " is not implemented yet",
                       binary_expr.location());
            assert(false);
        }

        assert(bin_op.has_value());
        if (is_constant) {
            auto * constant_lhs = llvm::dyn_cast<llvm::Constant>(lhs_value);
            auto * constant_rhs = llvm::dyn_cast<llvm::Constant>(rhs_value);
            store_result(llvm::ConstantExpr::get(*bin_op, constant_lhs, constant_rhs));
        } else {
            store_result(ir_builder->CreateBinOp(*bin_op, lhs_value, rhs_value));
        }
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

        auto * block = llvm::BasicBlock::Create(*context, func->getName(), func);
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

    void codegen::visit(ast::user_val & user_val) {
        using value_type = ast::user_val::value_type;
        switch (user_val.type) {
        case value_type::identifier: {
            auto * value = find_alive_value(user_val.val);
            if (value == nullptr) {
                printError("Could not find variable " + user_val.val);
                // TODO: Better recovery
                assert(false);
            }
            store_result(value);
        } break;
        case value_type::integer: {
            static constexpr auto hex_base = 16;
            static constexpr auto dec_base = 10;
            auto base = user_val.val.find_first_of('x') != std::string::npos ? hex_base : dec_base;
            store_result(
                llvm::ConstantInt::get(llvm::Type::getInt32Ty(*context), user_val.val, base));
        } break;
        case value_type::floating:
            std::cout << "Floating point IR not implemented" << std::endl;
            assert(false);
            break;
        case value_type::character:
            std::cout << "Character IR not implemented" << std::endl;
            assert(false);
            break;
        case value_type::boolean: {
            auto iter = valid_bools.find(user_val.val);
            assert(iter != valid_bools.end());
            store_result(llvm::ConstantInt::getBool(*context, iter->second));
        } break;
        case value_type::string:
            std::cout << "String IR not implemented" << std::endl;
            assert(false);
            break;
        }
    }
} // namespace visitor
