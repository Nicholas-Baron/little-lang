#ifndef _CONTEXT_MODULE_HPP
#define _CONTEXT_MODULE_HPP

#include "location.hpp"

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/Debug.h"

#include <algorithm>
#include <sstream>
#include <utility>

inline auto * find_local_value(llvm::Function *	func,
							   const std::string & name) {
	const auto & table = *(func->getValueSymbolTable());
	const auto   iter  = std::find_if(
		   table.begin(), table.end(),
		   [&name](const auto & entry) { return name == entry.getKey(); });

	if (iter != table.end()) { return iter->getValue(); }
	return table.lookup(name);
}

class context_module final {

	llvm::LLVMContext context_{};
	llvm::Module	  module_;
	llvm::IRBuilder<> builder_;

	std::vector<
		std::pair<llvm::Function *, std::map<std::string, llvm::Value *>>>
		currently_alive_values{};

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
			auto found = iter->second.find(name);
			if (found != iter->second.end()) { return found->second; }
		}

		auto * func = builder_.GetInsertBlock()->getParent();

		if (func != nullptr) { return find_local_value(func, name); }
		return find_first_class_value(name);
	}

	void verify_module() const { llvm::verifyModule(module_, &llvm::dbgs()); }

	void printError(const std::string & name, const Location * loc = nullptr) {

		if (loc == nullptr) {
			context_.emitError(name);
		} else {
			std::stringstream to_print{};
			to_print << *loc << " : " << name;
			context_.emitError(to_print.str());
		}
	}

	void add_value_to_table(const std::string & name, llvm::Value * val) {
		currently_alive_values.back().second.emplace(name, val);
	}

	auto * get_current_function() const {
		return currently_alive_values.back().first;
	}

	void add_new_scope(llvm::Function * parent) {
		if (parent == nullptr
			and currently_alive_values.back().first != nullptr) {
			parent = currently_alive_values.back().first;
		}

		currently_alive_values.emplace_back(
			parent, std::map<std::string, llvm::Value *>{});
	}

	void remove_current_scope() { currently_alive_values.pop_back(); }
};

#endif
