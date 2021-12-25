#include "type_context.hpp"

#include <llvm/IR/Type.h>

type_context::type_context(llvm::LLVMContext * context)
    : active_types{
        {ast::type{"int"}, llvm::Type::getInt32Ty(*context)},
        {ast::type{"float"}, llvm::Type::getFloatTy(*context)},
        {ast::type{"unit"}, llvm::Type::getVoidTy(*context)},
        {ast::type{"bool"}, llvm::Type::getInt1Ty(*context)},
        {ast::type{"char"}, llvm::Type::getInt8Ty(*context)},
        // TODO: Move to a Rust style 2 ptr string instead of a C style null-terminated string
        {ast::type{"string"}, llvm::Type::getInt8PtrTy(*context)},
    } {}

llvm::Type * type_context::lower_to_llvm(const ast::type & type) {
    auto iter = active_types.find(type);
    if (iter == active_types.end()) { return nullptr; }
    return iter->second;
}
