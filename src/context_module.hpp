#ifndef _CONTEXT_MODULE_HPP
#define _CONTEXT_MODULE_HPP

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/Support/Debug.h"

#include <iostream>

class context_module {

	llvm::LLVMContext context_{};
	llvm::Module	  module_;
	llvm::IRBuilder<> builder_;

   public:
	context_module() = delete;
	explicit context_module(const std::string & name)
		: module_{name, context_}, builder_{context_} {}

	context_module(const context_module &) = delete;
	context_module & operator=(const context_module &) = delete;

	context_module(context_module &&) = delete;
	context_module & operator=(context_module &&) = delete;

	~context_module() noexcept = default;

	auto & context() { return context_; }
	auto & module() { return module_; }
	auto & builder() { return builder_; }

	using int_type = std::pair<unsigned, bool>;

	void dump() const { module_.print(llvm::dbgs(), nullptr); }

	auto * find_first_class_value(const std::string & name) {
		return module_.getValueSymbolTable().lookup(name);
	}
};

inline auto * find_local_value(llvm::Function *	func,
							   const std::string & name) {
	return func->getValueSymbolTable()->lookup(name);
}

inline void print_symbol_table(llvm::ValueSymbolTable * table) {
	for (const auto & entry : *table) {
		std::cout << entry.getKey().str() << std::endl;
	}
}

#endif
