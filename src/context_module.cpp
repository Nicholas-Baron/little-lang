#include "context_module.hpp"

#include "llvm/IR/ValueSymbolTable.h"
#include "llvm/IR/Verifier.h"
#include "llvm/Support/raw_ostream.h"

#include <iostream> // cout
#include <sstream>  // stringstream

using llvm::Value, llvm::Type;

Value * find_local_value(llvm::Function * func, const std::string & name) {
    const auto * table = func->getValueSymbolTable();
    assert(table != nullptr);
    return table->lookup(name);
}

context_module::context_module(const std::string & name)
    : module_{std::make_unique<llvm::Module>(name, context_)}
    , builder_{context_}
    , valid_types{{"int", Type::getInt32Ty(context_)},
                  {"float", Type::getFloatTy(context_)},
                  {"proc", Type::getVoidTy(context_)},
                  {"bool", Type::getInt1Ty(context_)},
                  {"char", Type::getInt8Ty(context_)}}

{}

void context_module::dump() const {
    // TODO: move away from std::cout?
    std::string to_print;
    {
        llvm::raw_string_ostream stream(to_print);
        module_->print(stream, nullptr);
    }

    std::cout << to_print << std::endl;
}

Value * context_module::find_first_class_value(const std::string & name) const {
    return module_->getValueSymbolTable().lookup(name);
}

llvm::Value * context_module::find_value_in_current_scope(const std::string & name) {
    for (auto iter = currently_alive_values.rbegin(); iter != currently_alive_values.rend();
         iter++) {
        if (auto found = iter->find(name); found != iter->end()) { return found->second; }
    }

    if (auto iter = constants.find(name); iter != constants.end()) {
        if (iter->second->getType()->isPointerTy()) {
            return builder().CreateLoad(iter->second->getType()->getPointerElementType(),
                                        iter->second);
        }
        return iter->second;
    }

    auto * func = builder_.GetInsertBlock()->getParent();

    if (func != nullptr) { return find_local_value(func, name); }
    return find_first_class_value(name);
}

void context_module::verify_module() const { llvm::verifyModule(*module_, &llvm::errs()); }

llvm::FunctionCallee context_module::find_function(const std::string & name) {
    return module_->getFunction(name);
}

void context_module::printError(const std::string & name, std::optional<Location> loc) {

    if (loc == std::nullopt) {
        context_.emitError(name);
    } else {
        std::stringstream to_print;
        to_print << *loc << " : " << name;
        context_.emitError(to_print.str());
    }
}

llvm::Type * context_module::get_identifer_type(const std::string & name) {
    // check constants
    if (auto iter = constants.find(name); iter != constants.end()) {
        return iter->second->getType();
    }

    // check functions
    if (auto * func = module().getFunction(name); func != nullptr) {
        return func->getFunctionType();
    }

    // check locals
    return find_value_in_current_scope(name)->getType();
}
