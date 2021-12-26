#include "type_context.hpp"

#include <llvm/IR/Type.h>

type_context::type_context(llvm::LLVMContext * context)
    : active_types{
        {std::make_shared<ast::prim_type>(ast::prim_type::type::int32),
         llvm::Type::getInt32Ty(*context)},
        {std::make_shared<ast::prim_type>(ast::prim_type::type::float32),
         llvm::Type::getFloatTy(*context)},
        {std::make_shared<ast::prim_type>(ast::prim_type::type::unit),
         llvm::Type::getVoidTy(*context)},
        {std::make_shared<ast::prim_type>(ast::prim_type::type::boolean),
         llvm::Type::getInt1Ty(*context)},
        {std::make_shared<ast::prim_type>(ast::prim_type::type::character),
         llvm::Type::getInt8Ty(*context)},
        // TODO: Move to a Rust style 2 ptr string instead of a C style null-terminated string
        {std::make_shared<ast::prim_type>(ast::prim_type::type::str),
         llvm::Type::getInt8PtrTy(*context)},
    } {}

llvm::Type * type_context::lower_to_llvm(const ast::type & type) {
    auto iter = std::find_if(active_types.begin(), active_types.end(),
                             [&type](const auto & entry) -> bool { return *entry.first == type; });
    if (iter == active_types.end()) { return nullptr; }
    return iter->second;
}
