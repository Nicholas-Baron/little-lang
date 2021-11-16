#include "nodes.hpp"

#include "context_module.hpp"
#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "parser.hpp"
#include <llvm/IR/GlobalVariable.h>

#include <cassert>
#include <cctype> // isdigit
#include <vector>

using llvm::Type, llvm::FunctionType, llvm::Value;

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

    [[nodiscard]] std::string temp_block_name() {
        static unsigned num = 0;
        return "block_" + std::to_string(num++);
    }

    [[nodiscard]] bool terminated(llvm::BasicBlock * block) { return block->back().isTerminator(); }

    [[nodiscard]] Value * comparison_expr(context_module & context, int tok, Value * const left,
                                          Value * const right, std::optional<Location> loc) {

        if (left->getType() != right->getType()) {
            context.printError("Current compiler does not support comparisons on differing types.",
                               loc);
            return context.builder().getFalse();
        }

        const bool is_int = left->getType()->isIntegerTy();
        switch (tok) {
        case T_LE:
            if (is_int) {
                return context.builder().CreateICmpSLE(left, right);
            } else {
                return context.builder().CreateFCmpOLE(left, right);
            }

        case T_LT:
            if (is_int) {
                return context.builder().CreateICmpSLT(left, right);
            } else {
                return context.builder().CreateFCmpOLT(left, right);
            }

        case T_GT:
            if (is_int) {
                return context.builder().CreateICmpSGT(left, right);
            } else {
                return context.builder().CreateFCmpOGT(left, right);
            }

        case T_EQ:
            if (is_int) {
                return context.builder().CreateICmpEQ(left, right);
            } else {
                return context.builder().CreateFCmpOEQ(left, right);
            }
        }

        context.printError("Token number " + tok_to_string(tok)
                               + " is not currently supported as a comparison.",
                           loc);
        return context.builder().getFalse();
    }

    [[nodiscard]] Value * short_circuit(context_module & context, Expression * lhs, int tok,
                                        Expression * rhs) {

        assert(tok == T_AND or tok == T_OR);
        auto * left = lhs->codegen(context);
        auto * lhs_block = context.builder().GetInsertBlock();

        auto * rhs_block = llvm::BasicBlock::Create(context.context(), temp_block_name(),
                                                    context.get_current_function());

        auto * merge_block = llvm::BasicBlock::Create(context.context(), temp_block_name(),
                                                      context.get_current_function());

        // add a check to short circuit
        // short on false if anding, short on true if oring

        // if (true && rhs) -> should eval rhs
        auto * on_true = tok == T_AND ? rhs_block : merge_block;

        // if (false || rhs) -> should eval rhs
        auto * on_false = tok == T_OR ? rhs_block : merge_block;
        assert(on_true != on_false);

        context.builder().CreateCondBr(left, on_true, on_false);

        context.builder().SetInsertPoint(rhs_block);
        auto * right = rhs->codegen(context);
        context.builder().CreateBr(merge_block);

        context.builder().SetInsertPoint(merge_block);
        auto * phi = context.builder().CreatePHI(left->getType(), 2);
        phi->addIncoming(left, lhs_block);
        phi->addIncoming(right, rhs_block);

        return phi;
    }

    const std::map<std::string, bool> valid_bools{{"true", true},   {"True", true},
                                                  {"TRUE", true},   {"false", false},
                                                  {"False", false}, {"FALSE", false}};

} // namespace

std::vector<Type *> Func_Header::param_types(context_module & context) {

    std::vector<Type *> to_ret;
    to_ret.reserve(params.size());

    for (const auto & param : params) { to_ret.push_back(context.find_type(param.type(), loc)); }

    return to_ret;
}

FunctionType * Func_Header::full_type(context_module & context) {

    if (ret_type.empty() or ret_type == "auto") {
        context.printError(name_ + " does not have a known return type", location());
    }

    return FunctionType::get(context.find_type(ret_type, location()), param_types(context), false);
}

llvm::ConstantInt * UserValue::as_i32(context_module & context) const {
    static constexpr auto hex_base = 16;
    static constexpr auto dec_base = 10;
    auto base = val.find_first_of('x') != std::string::npos ? hex_base : dec_base;
    return context.builder().getInt32(std::stoi(val, nullptr, base));
}

llvm::ConstantInt * UserValue::as_bool(context_module & context) const {
    auto iter = valid_bools.find(val);
    assert(iter != valid_bools.end());
    return context.builder().getInt1(iter->second);
}

bool UserValue::is_bool() const { return valid_bools.find(val) != valid_bools.end(); }

// TODO: Remove duplication
Value * UserValue::codegen(context_module & context) {
    const auto first_char = val.at(0);

    switch (first_char) {
    // A user-defined string
    case '\"':
        return context.builder().CreateGlobalString(val);

    // A single character
    case '\'':
        return context.builder().getInt8(val[1]);
    }

    if (isdigit(first_char) != 0) {
        // Some number

        using std::stoi, std::stof;
        if (val.find_first_of('.') != std::string::npos) {
            // Floating point
            return llvm::ConstantFP::get(context.context(), llvm::APFloat{stof(val)});
        }

        // Decimal integer
        return as_i32(context);
    }

    // Some identifier or bool
    if (is_bool()) { return as_bool(context); }

    // Identifier
    auto * value = context.find_value_in_current_scope(val);
    if (value == nullptr) {
        context.printError("Could not find variable named " + val, location());
    }
    return value;
}

llvm::Constant * UserValue::compile_time_codegen(context_module & context) {

    const auto first_char = val.at(0);

    switch (first_char) {
    // A user-defined string
    case '\"':
        return context.builder().CreateGlobalString(val);

    // A single character
    case '\'':
        return context.builder().getInt8(val[1]);
    }

    if (isdigit(first_char) != 0) {
        // Some number

        using std::stoi, std::stof;
        if (val.find_first_of('.') != std::string::npos) {
            // Floating point
            return llvm::ConstantFP::get(context.context(), llvm::APFloat{stof(val)});
        }

        // Decimal integer
        return as_i32(context);
    }

    // Some identifier or bool
    if (is_bool()) { return as_bool(context); }

    // some identifier
    return context.get_constant(val);
}

Value * UnaryExpression::codegen(context_module & context) {

    auto * op_value = expr->codegen(context);

    switch (tok) {
    case T_NOT:
        return context.builder().CreateNot(op_value);
    case T_MINUS:
        return context.builder().CreateNeg(op_value);
    }

    context.printError("Token number " + tok_to_string(tok)
                           + " is not an implemented unary operation.",
                       location());
    return op_value;
}

llvm::Constant * UnaryExpression::compile_time_codegen(context_module & context) {
    auto * op_value = expr->compile_time_codegen(context);
    switch (tok) {
    case T_NOT:
        return llvm::ConstantExpr::getNot(op_value);
    case T_MINUS:
        return op_value->getType()->isFloatingPointTy() ? llvm::ConstantExpr::getFNeg(op_value)
                                                        : llvm::ConstantExpr::getNeg(op_value);
    }
    context.printError("Token number " + tok_to_string(tok)
                           + " is not an implemented compile time unary operation.",
                       location());
    return op_value;
}

bool BinaryExpression::is_comparison() const noexcept {
    switch (tok) {
    case T_GE:
    case T_GT:
    case T_LT:
    case T_LE:
    case T_EQ:
    case T_NE:
        return true;
    default:
        return false;
    }
}

bool BinaryExpression::is_shortcircuiting() const noexcept { return tok == T_OR or tok == T_AND; }

Value * BinaryExpression::codegen(context_module & context) {

    if (is_shortcircuiting()) { return short_circuit(context, lhs_.get(), tok, rhs_.get()); }

    auto * left = lhs_->codegen(context);
    auto * right = rhs_->codegen(context);

    if (left == nullptr) {
        context.printError("Token #" + tok_to_string(tok) + " has a null left operand.",
                           location());
        return right;
    }

    if (right == nullptr) {
        context.printError("Token #" + tok_to_string(tok) + " has a null right operand.",
                           location());
        return left;
    }

    if (is_comparison()) { return comparison_expr(context, tok, left, right, location()); }

    switch (tok) {
    case T_PLUS:
        return context.builder().CreateAdd(left, right);
    case T_MINUS:
        return context.builder().CreateSub(left, right);
    case T_MULT:
        return context.builder().CreateMul(left, right);
    }

    context.printError("Token number " + tok_to_string(tok)
                           + " is not an implemented binary operation.",
                       location());
    return nullptr;
}

llvm::Constant * BinaryExpression::compile_time_codegen(context_module & context) {
    auto * left = lhs_->compile_time_codegen(context);
    auto * right = rhs_->compile_time_codegen(context);

    if (left == nullptr) {
        context.printError("Token #" + tok_to_string(tok) + " has a null left operand.",
                           location());
        return right;
    }

    if (right == nullptr) {
        context.printError("Token #" + tok_to_string(tok) + " has a null right operand.",
                           location());
        return left;
    }

    switch (tok) {
    case T_PLUS:
        return llvm::ConstantExpr::getAdd(left, right);
    case T_MINUS:
        return llvm::ConstantExpr::getSub(left, right);
    case T_MULT:
        return llvm::ConstantExpr::getMul(left, right);
    }

    context.printError("Token number " + tok_to_string(tok)
                           + " is not an implemented binary operation.",
                       location());
    return nullptr;
}

Value * FunctionCall::codegen(context_module & context) {
    auto callee = context.find_function(name_);

    std::vector<Value *> arg_values{};
    arg_values.reserve(args_.size());
    for (auto & arg : args_) { arg_values.push_back(arg->codegen(context)); }

    return context.builder().CreateCall(callee, arg_values);
}

Value * If_Statement::codegen(context_module & context) {

    auto * cond = condition->codegen(context);
    auto * start_block = context.builder().GetInsertBlock();

    auto * then_block = context.create_new_insertion_point(temp_block_name());
    true_branch->codegen(context);

    if (else_branch == nullptr) {

        auto * merge_block = llvm::BasicBlock::Create(context.context(), temp_block_name(),
                                                      context.get_current_function());

        if (not terminated(start_block)) {
            context.builder().SetInsertPoint(start_block);
            context.builder().CreateCondBr(cond, then_block, merge_block);
        }
        context.builder().SetInsertPoint(merge_block);

        return nullptr;
    }
    auto * else_block = context.create_new_insertion_point(temp_block_name());
    else_branch->codegen(context);

    context.builder().SetInsertPoint(start_block);
    context.builder().CreateCondBr(cond, then_block, else_block);

    if (not terminated(else_block) and not terminated(then_block)) {
        auto * merge_block = llvm::BasicBlock::Create(context.context(), temp_block_name(),
                                                      context.get_current_function());
        if (not terminated(then_block)) {
            context.builder().SetInsertPoint(then_block);

            context.builder().CreateBr(merge_block);
        }

        context.builder().SetInsertPoint(merge_block);
    }

    return nullptr;
}

Value * Let_Statement::codegen(context_module & context) {

    auto * value = value_->codegen(context);
    auto type = name_and_type.type();
    if (type == "auto" or context.find_type(type, location()) == value->getType()) {
        value->setName(name_and_type.name());
        return value;
    }

    context.printError("Casting is not supported at this time.", location());
    return nullptr;
}

Value * Return_Statement::codegen(context_module & context) {
    if (value == nullptr) { return context.builder().CreateRetVoid(); }

    return context.builder().CreateRet(value->codegen(context));
}

void Func_Header::add_parameters(context_module & context, llvm::Function & func) const {

    unsigned index = 0;
    auto * const args_end = func.arg_end();
    for (auto * arg = func.arg_begin(); arg != args_end; arg++, index++) {
        assert(arg != nullptr);
        const auto & arg_name = this->arg(index).name();
        assert(not arg_name.empty());

        arg->setName(arg_name);
        context.add_value_to_table(arg_name, arg);
    }
}

Value * Function::codegen(context_module & context) {

    auto * func_type = head_.full_type(context);

    assert(func_type != nullptr);

    auto * func = context.create_new_function(func_type, head_.name());
    context.create_new_insertion_point(head_.name() + "_start", func);
    head_.add_parameters(context, *func);
    body_->codegen(context);
    context.remove_current_scope();

    llvm::verifyFunction(*func, &llvm::dbgs());
    return func;
}

Value * Constant::codegen(context_module & context) {
    auto * value = expr->compile_time_codegen(context);
    auto * global = new llvm::GlobalVariable{context.module(),
                                             value->getType(),
                                             true,
                                             llvm::GlobalVariable::LinkageTypes::ExternalLinkage,
                                             value,
                                             name_and_type.name()};
    context.insert_constant(name_and_type.name(), global);
    return nullptr;
}
