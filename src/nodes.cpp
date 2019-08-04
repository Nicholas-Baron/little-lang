#include "nodes.hpp"

#include "parser.hpp"

#include "llvm/IR/Type.h"

#include <algorithm>
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
}  // namespace

std::vector<Type *> Func_Header::param_types(context_module & context) {

	std::vector<Type *> to_ret{};
	to_ret.reserve(params.size());

	for (const auto & param : params) {
		to_ret.push_back(get_type_by_name(param.name(), context.context()));
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

Value * Function::codegen(context_module & context) {

	auto * func_type = head_.full_type(context);

	auto * func
		= llvm::Function::Create(func_type, llvm::Function::ExternalLinkage,
								 head_.name(), &context.module());

	for (unsigned arg_index = 0; arg_index < func_type->getNumParams();
		 arg_index++) {
		(func->arg_begin() + arg_index)->setName(head_.arg(arg_index).name());
	}

	return func;
}
