#include "context_module.hpp"

#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"

#include <algorithm>  // find_if
#include <iostream>   // cout
#include <sstream>	// stringstream

using llvm::Value, llvm::Type;

Value * find_local_value(llvm::Function * func, const std::string & name) {
	const auto & table = *(func->getValueSymbolTable());
	const auto   iter  = std::find_if(
		   table.begin(), table.end(),
		   [&name](const auto & entry) { return name == entry.getKey(); });

	if (iter != table.end()) { return iter->getValue(); }
	return table.lookup(name);
}

context_module::context_module(const std::string & name)
	: module_{name, context_}
	, builder_{context_}
	, valid_types{{"int", Type::getInt32Ty(context_)},
				  {"float", Type::getFloatTy(context_)},
				  {"proc", Type::getVoidTy(context_)},
				  {"bool", Type::getInt1Ty(context_)},
				  {"char", Type::getInt8Ty(context_)}}

{}

void context_module::dump() const {
	std::string to_print;
	{
		llvm::raw_string_ostream stream(to_print);
		module_.print(stream, nullptr);
	}

	std::cout << to_print << std::endl;
}

Value * context_module::find_first_class_value(const std::string & name) const {
	return module_.getValueSymbolTable().lookup(name);
}

void context_module::verify_module() const {
	llvm::verifyModule(module_, &llvm::dbgs());
}
void context_module::printError(const std::string & name,
								const Location *	loc) {

	if (loc == nullptr) {
		context_.emitError(name);
	} else {
		std::stringstream to_print{};
		to_print << *loc << " : " << name;
		context_.emitError(to_print.str());
	}
}
