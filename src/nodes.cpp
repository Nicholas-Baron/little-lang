#include "nodes.hpp"

#include "parser.hpp"

#include "llvm/ADT/APFloat.h"
#include "llvm/IR/Type.h"

#include <algorithm>
#include <cassert>
#include <cctype>
#include <functional>
#include <vector>

using llvm::Type, llvm::LLVMContext, llvm::FunctionType, llvm::Value;

namespace {
	Type * get_type_by_name(const std::string & name, LLVMContext & context) {

		// TODO: Move into the context_module
		static const std::vector<
			std::pair<std::string, std::function<Type *(LLVMContext &)>>>
			primitive_types{{"int", Type::getInt32Ty},
							{"float", Type::getFloatTy},
							{"void", Type::getVoidTy},
							{"bool", Type::getInt1Ty},
							{"char", Type::getInt8Ty}};

		const auto to_ret = std::find_if(
			primitive_types.begin(), primitive_types.end(),
			[&name](const auto & entry) { return entry.first == name; });

		if (to_ret == primitive_types.end()) {
			context.emitError(name + " is an unknown type");
			return nullptr;
		}
		return to_ret->second(context);
	}

	std::string temp_block_name() {
		static unsigned num = 0;
		return "block_" + std::to_string(num++);
	}
}  // namespace

std::vector<Type *> Func_Header::param_types(context_module & context) {

	std::vector<Type *> to_ret{};
	to_ret.reserve(params.size());

	for (const auto & param : params) {
		to_ret.push_back(get_type_by_name(param.type(), context.context()));
	}

	return to_ret;
}

FunctionType * Func_Header::full_type(context_module & context) {

	if (ret_type.empty() or ret_type == "auto") {
		context.context().emitError(name_
									+ " does not have a known return type");
	}

	return FunctionType::get(get_type_by_name(ret_type, context.context()),
							 param_types(context), false);
}

Value * UserValue::codegen(context_module & context) {
	auto first_char = val.at(0);

	using std::string;
	if (isdigit(first_char) != 0) {
		// Some number

		using std::stoi, std::stof;
		if (val.find_first_of('x') != string::npos) {
			// Hex number
			static constexpr auto hex_base = 16;
			return context.builder().getInt32(stoi(val, nullptr, hex_base));
		} else if (val.find_first_of('.') != string::npos) {
			// Floating point
			return llvm::ConstantFP::get(context.context(),
										 llvm::APFloat{stof(val)});
		} else {
			// Decimal integer
			return context.builder().getInt32(stoi(val));
		}

	} else if (first_char == '\'') {
		// A single character
		return context.builder().getInt8(val[1]);
	} else if (first_char == '"') {
		// A string
		return context.builder().CreateGlobalString(val);
	} else {
		// Some identifier or bool
		static const std::map<string, bool> valid_bools{
			{"true", true},   {"True", true},   {"TRUE", true},
			{"false", false}, {"False", false}, {"FALSE", false}};

		const auto bool_value = valid_bools.find(val);

		if (bool_value != valid_bools.end()) {
			// Boolean value
			return context.builder().getInt1(bool_value->second);
		} else {
			// Identifier
			return context.find_value_in_current_scope(val);
		}
	}
}

Value * UnaryExpression::codegen(context_module & context) {

	auto * op_value = expr->codegen(context);

	switch (tok) {
		case T_NOT:
			return context.builder().CreateNot(op_value);
	}

	context.context().emitError("Token number " + std::to_string(tok)
								+ " is not an implemented unary operation.");
	return op_value;
}

Value * comparison_expr(context_module & context, int tok, Value * left,
						Value * right) {

	if (left->getType() != right->getType()) {
		context.context().emitError(
			"Current compiler does not support comparisons on differing "
			"types.");
		return context.builder().getFalse();
	}

	if (tok == T_LE) {
		if (left->getType()->isIntegerTy()) {
			return context.builder().CreateICmpSLE(left, right);
		} else {
			return context.builder().CreateFCmpOLE(left, right);
		}
	}

	context.context().emitError(
		"Token number " + std::to_string(tok)
		+ " is not currently supported as a comparison.");
	return context.builder().getFalse();
}

Value * BinaryExpression::codegen(context_module & context) {
	auto * left  = lhs_->codegen(context);
	auto * right = rhs_->codegen(context);

	switch (tok) {
		case T_PLUS:
			return context.builder().CreateAdd(left, right);
		case T_MINUS:
			return context.builder().CreateSub(left, right);
		case T_MULT:
			return context.builder().CreateMul(left, right);
		case T_GE:
		case T_GT:
		case T_LT:
		case T_LE:
		case T_EQ:
		case T_NE:
			return comparison_expr(context, tok, left, right);
	}

	context.context().emitError("Token number " + std::to_string(tok)
								+ " is not an implemented binary operation.");
	return right;
}

Value * If_Statement::codegen(context_module & context) {

	auto * cond		   = condition->codegen(context);
	auto * start_block = context.builder().GetInsertBlock();

	auto * then_block
		= llvm::BasicBlock::Create(context.context(), temp_block_name());
	context.builder().SetInsertPoint(then_block);
	true_branch->codegen(context);

	if (else_branch == nullptr) {

		auto * merge_block
			= llvm::BasicBlock::Create(context.context(), temp_block_name());

		context.builder().SetInsertPoint(start_block);
		auto * brancher
			= context.builder().CreateCondBr(cond, then_block, merge_block);
		context.builder().SetInsertPoint(merge_block);

		return nullptr;
	} else {
		auto * else_block
			= llvm::BasicBlock::Create(context.context(), temp_block_name());
		else_branch->codegen(context);

		auto * merge_block
			= llvm::BasicBlock::Create(context.context(), temp_block_name());

		context.builder().SetInsertPoint(start_block);
		auto * brancher
			= context.builder().CreateCondBr(cond, then_block, merge_block);
		context.builder().SetInsertPoint(then_block);
		context.builder().CreateBr(merge_block);
		context.builder().SetInsertPoint(merge_block);

		return nullptr;
	}
}
Value * Return_Statement::codegen(context_module & context) {
	if (value == nullptr) { return context.builder().CreateRetVoid(); }

	auto * val = value->codegen(context);
	return context.builder().CreateRet(val);
}

Value * Function::codegen(context_module & context) {

	auto * func_type = head_.full_type(context);

	assert(func_type != nullptr);

	auto * func
		= llvm::Function::Create(func_type, llvm::Function::ExternalLinkage,
								 head_.name(), &context.module());

	{
		unsigned   index	= 0;
		const auto args_end = func->arg_end();
		for (auto arg = func->arg_begin(); arg != args_end; arg++, index++) {
			assert(arg != nullptr);
			arg->setName(head_.arg(index).name());
		}
	}

	auto * blk = llvm::BasicBlock::Create(context.context(),
										  head_.name() + "_start", func);
	context.builder().SetInsertPoint(blk);

	body_->codegen(context);

	return func;
}
