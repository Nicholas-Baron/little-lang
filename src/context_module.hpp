#ifndef _CONTEXT_MODULE_HPP
#define _CONTEXT_MODULE_HPP

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "location.hpp"

#include <map>
#include <utility>
#include <vector>

llvm::Value * find_local_value(llvm::Function * func, const std::string & name);

class context_module final {

    llvm::LLVMContext context_{};
    std::unique_ptr<llvm::Module> module_;
    llvm::IRBuilder<> builder_;

    std::vector<std::pair<llvm::Function *, std::map<std::string, llvm::Value *>>>
        currently_alive_values{};

    std::map<std::string, llvm::Type *> valid_types;

  public:
    context_module() = delete;
    explicit context_module(const std::string & name);

    context_module(const context_module &) = delete;
    context_module & operator=(const context_module &) = delete;

    context_module(context_module &&) = delete;
    context_module & operator=(context_module &&) = delete;

    ~context_module() noexcept = default;

    auto & context() { return context_; }
    llvm::Module & module() {
        assert(module_ != nullptr);
        return *module_;
    }
    auto & builder() { return builder_; }

    std::unique_ptr<llvm::Module> take_module() && { return std::move(module_); }

    void dump() const;

    llvm::Value * find_first_class_value(const std::string & name) const;

    auto * find_value_in_current_scope(const std::string & name) {
        for (auto iter = currently_alive_values.rbegin(); iter != currently_alive_values.rend();
             iter++) {
            auto found = iter->second.find(name);
            if (found != iter->second.end()) { return found->second; }
        }

        auto * func = builder_.GetInsertBlock()->getParent();

        if (func != nullptr) { return find_local_value(func, name); }
        return find_first_class_value(name);
    }

    void verify_module() const;

    void printError(const std::string & name, const Location * loc = nullptr);

    void add_value_to_table(const std::string & name, llvm::Value * val) {
        currently_alive_values.back().second.emplace(name, val);
    }

    auto * get_current_function() const { return currently_alive_values.back().first; }

    llvm::Function * find_function(const std::string & name) { return module_->getFunction(name); }

    void add_new_scope(llvm::Function * parent) {
        if (parent == nullptr and currently_alive_values.back().first != nullptr) {
            parent = currently_alive_values.back().first;
        }

        currently_alive_values.emplace_back(parent, std::map<std::string, llvm::Value *>{});
    }

    void remove_current_scope() { currently_alive_values.pop_back(); }

    llvm::Type * find_type(const std::string & name, const Location * loc) {

        const auto iter = valid_types.find(name);
        if (iter != valid_types.end()) { return iter->second; }
        printError(name + " is an unknown type", loc);
        return nullptr;
    }
};

#endif
