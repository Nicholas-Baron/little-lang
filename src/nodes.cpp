#include "nodes.hpp"

#include "parser.hpp"

#include "llvm/IR/Type.h"

using llvm::Type, llvm::LLVMContext;

namespace {
	Type * get_type_by_name(const std::string & name, LLVMContext & context) {
		if (name == "int") {
			return Type::getInt32Ty(context);
		} else if (name == "float") {
			return Type::getFloatTy(context);
		} else if (name == "bool") {
			return Type::getInt1Ty(context);
		} else if (name == "proc") {
			return Type::getVoidTy(context);
		} else if (name == "char") {
			return Type::getInt8Ty(context);
		} else {
			context.emitError("Unknown type: " + name);
			return nullptr;
		}
	}
}  // namespace
