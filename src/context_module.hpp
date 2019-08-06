#ifndef _CONTEXT_MODULE_HPP
#define _CONTEXT_MODULE_HPP

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/Support/Debug.h"

#include <iostream>

inline auto * find_local_value(llvm::Function *	func,
							   const std::string & name) {
	for (const auto & entry : *(func->getValueSymbolTable())) {
		if (entry.getKey() == name) {
			return entry.getValue();
		} else {
			unsigned index = 0;
			for (; entry.getKey()[index] == name[index]
				   and index < entry.getKey().size() and index < name.size();
				 index++) {}
			std::cout << entry.getKey().str() << " and " << name
					  << " differ at " << index << std::endl;
		}
	}
	return func->getValueSymbolTable()->lookup(name);
}

inline void print_symbol_table(llvm::ValueSymbolTable * table) {
	for (const auto & entry : *table) {
		std::cout << entry.getKey().str() << std::endl;
	}
}

class context_module {

	llvm::LLVMContext context_{};
	llvm::Module	  module_;
	llvm::IRBuilder<> builder_;

	std::vector<std::map<std::string, llvm::Value *>> currently_alive_values{};

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

	void dump() const { module_.print(llvm::dbgs(), nullptr); }

	auto * find_first_class_value(const std::string & name) const {
		return module_.getValueSymbolTable().lookup(name);
	}

	auto * find_value_in_current_scope(const std::string & name) {
		for (auto iter = currently_alive_values.rbegin();
			 iter != currently_alive_values.rend(); iter++) {
			auto found = iter->find(name);
			if (found != iter->end()) { return found->second; }
		}

		auto * func = builder_.GetInsertBlock()->getParent();

		if (func != nullptr) {
			std::cout << "Searching for " << name << " in function "
					  << func->getName().str() << std::endl;
			return find_local_value(func, name);
		} else {
			return find_first_class_value(name);
		}
	}

	void printError(const std::string & name) { context_.emitError(name); }

	void register_value(const std::string & name, llvm::Value * val) {
		currently_alive_values.back().emplace(name, val);
	}

	void add_new_scope() { currently_alive_values.emplace_back(); }

	void remove_current_scope() { currently_alive_values.pop_back(); }
};

#endif
