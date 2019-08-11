#include "context_module.hpp"

#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Debug.h"

#include <algorithm>
#include <sstream>

using llvm::Value;

Value * find_local_value(llvm::Function * func, const std::string & name) {
	const auto & table = *(func->getValueSymbolTable());
	const auto   iter  = std::find_if(
		   table.begin(), table.end(),
		   [&name](const auto & entry) { return name == entry.getKey(); });

	if (iter != table.end()) { return iter->getValue(); }
	return table.lookup(name);
}

void context_module::dump() const { module_.print(llvm::dbgs(), nullptr); }

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
