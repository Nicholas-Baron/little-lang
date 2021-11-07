#include "nodes.hpp"

#include "llvm/IR/Type.h"
#include "llvm/IR/Verifier.h"
#include "parser.hpp"

#include <cassert>
#include <cctype> // isdigit
#include <vector>

using llvm::Type, llvm::FunctionType, llvm::Value;

namespace {
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

        context.printError("Token number " + std::to_string(tok)
                               + " is not currently supported as a comparison.",
                           loc);
        return context.builder().getFalse();
    }

    [[nodiscard]] Value * short_circuit(context_module & context, Expression * lhs, int tok,
                                        Expression * rhs, std::optional<Location> loc) {

        assert(tok == T_AND or tok == T_OR);
        auto * left = lhs->codegen(context);

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
        switch (tok) {
        case T_AND:
            return context.builder().CreateAnd(left, right);
        case T_OR:
            return context.builder().CreateOr(left, right);
        default:
            context.printError("Unimplemented shortcircuiting token " + std::to_string(tok), loc);
            assert(false);
        }
    }

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
        if (val.find_first_of('x') != std::string::npos) {
            // Hex number
            static constexpr auto hex_base = 16;
            return context.builder().getInt32(stoi(val, nullptr, hex_base));
        }

        if (val.find_first_of('.') != std::string::npos) {
            // Floating point
            return llvm::ConstantFP::get(context.context(), llvm::APFloat{stof(val)});
        }

        // Decimal integer
        return context.builder().getInt32(stoi(val));
    }

    // Some identifier or bool
    static const std::map<std::string, bool> valid_bools{{"true", true},   {"True", true},
                                                         {"TRUE", true},   {"false", false},
                                                         {"False", false}, {"FALSE", false}};

    if (const auto bool_value = valid_bools.find(val); bool_value != valid_bools.end()) {
        // Boolean value
        return context.builder().getInt1(bool_value->second);
    }

    // Identifier
    auto * value = context.find_value_in_current_scope(val);
    if (value == nullptr) {
        context.printError("Could not find variable named " + val, location());
    }
    return value;
}

Value * UnaryExpression::codegen(context_module & context) {

    auto * op_value = expr->codegen(context);

    switch (tok) {
    case T_NOT:
        return context.builder().CreateNot(op_value);
    case T_MINUS:
        return context.builder().CreateNeg(op_value);
    }

    context.printError("Token number " + std::to_string(tok)
                           + " is not an implemented unary operation.",
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

    if (is_shortcircuiting()) {
        return short_circuit(context, lhs_.get(), tok, rhs_.get(), location());
    }

    auto * left = lhs_->codegen(context);
    auto * right = rhs_->codegen(context);

    if (left == nullptr) {
        context.printError("Token #" + std::to_string(tok) + " has a null left operand.",
                           location());
        return right;
    }

    if (right == nullptr) {
        context.printError("Token #" + std::to_string(tok) + " has a null right operand.",
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

    context.printError("Token number " + std::to_string(tok)
                           + " is not an implemented binary operation.",
                       location());
    return nullptr;
}

Value * FunctionCall::codegen(context_module & context) {
    auto * callee = context.find_function(name_);

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

Value * Function::codegen(context_module & context) {

    auto * func_type = head_.full_type(context);

    assert(func_type != nullptr);

    auto * func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, head_.name(),
                                         &context.module());

    context.add_new_scope(func);

    {
        unsigned index = 0;
        auto * const args_end = func->arg_end();
        for (auto * arg = func->arg_begin(); arg != args_end; arg++, index++) {
            assert(arg != nullptr);
            const auto & arg_name = head_.arg(index).name();
            assert(not arg_name.empty());
            arg->setName(arg_name);

            context.add_value_to_table(arg_name, arg);
        }
    }

    context.create_new_insertion_point(head_.name() + "_start");

    body_->codegen(context);

    context.remove_current_scope();
    llvm::verifyFunction(*func, &llvm::dbgs());
    return func;
}
