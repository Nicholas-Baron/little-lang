#ifndef _CONTEXT_MODULE_HPP
#define _CONTEXT_MODULE_HPP

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "location.hpp"
#include "utils/move_copy.hpp"

#include <map>
#include <optional>
#include <utility>
#include <vector>

class context_module final {

    llvm::LLVMContext context_{};
    // NOTE: This field is never null, but is out of line to allow the module to be moved from us.
    std::unique_ptr<llvm::Module> module_;
    llvm::IRBuilder<> builder_;

    std::vector<std::map<std::string, llvm::Value *>> currently_alive_values;

    std::map<std::string, llvm::Type *> valid_types;
    std::map<std::string, llvm::Constant *> constants;

  public:
    context_module() = delete;
    explicit context_module(const std::string & name);

    non_copyable(context_module);

    non_movable(context_module);

    ~context_module() noexcept = default;

    auto & context() { return context_; }
    llvm::Module & module() {
        assert(module_ != nullptr);
        return *module_;
    }
    auto & builder() { return builder_; }

    std::unique_ptr<llvm::Module> take_module() && noexcept { return std::move(module_); }

    void dump() const;

    [[nodiscard]] llvm::Value * find_first_class_value(const std::string & name) const;

    llvm::Value * find_value_in_current_scope(const std::string & name);

    void verify_module() const;

    void printError(const std::string & name, std::optional<Location> loc = std::nullopt);

    void add_value_to_table(const std::string & name, llvm::Value * val) {
        currently_alive_values.back().emplace(name, val);
    }

    [[nodiscard]] llvm::Function * get_current_function() const {
        auto * current_block = builder_.GetInsertBlock();
        assert(current_block != nullptr);
        return current_block->getParent();
    }

    llvm::FunctionCallee find_function(const std::string & name);

    void add_new_scope() {
        currently_alive_values.emplace_back(std::map<std::string, llvm::Value *>{});
    }

    llvm::BasicBlock * create_new_insertion_point(const std::string & block_name,
                                                  llvm::Function * parent = nullptr) {
        auto * block = llvm::BasicBlock::Create(
            context(), block_name, parent != nullptr ? parent : get_current_function());
        builder().SetInsertPoint(block);
        return block;
    }

    llvm::Function * create_new_function(llvm::FunctionType * func_type, const std::string & name) {

        auto * func = llvm::Function::Create(func_type, llvm::Function::ExternalLinkage, name,
                                             module_.get());
        add_new_scope();
        return func;
    }

    void remove_current_scope() { currently_alive_values.pop_back(); }

    llvm::Type * find_type(const std::string & name, std::optional<Location> loc) {

        const auto iter = valid_types.find(name);
        if (iter != valid_types.end()) { return iter->second; }
        printError(name + " is an unknown type", loc);
        return nullptr;
    }

    void insert_constant(std::string name, llvm::Constant * value) {
        constants.emplace(std::move(name), value);
    }

    llvm::Constant * get_constant(const std::string & name) {
        auto iter = constants.find(name);
        if (iter != constants.end()) { return iter->second; }

        printError("Could not find constant " + name);
        return nullptr;
    }
};

#endif
