#include "type_context.hpp"

#include <llvm/IR/DerivedTypes.h>

type_context::type_context(llvm::LLVMContext * context)
    : active_types{
        {ast::prim_type::int32, llvm::Type::getInt32Ty(*context)},
        {ast::prim_type::float32, llvm::Type::getFloatTy(*context)},
        {ast::prim_type::unit, llvm::Type::getVoidTy(*context)},
        {ast::prim_type::boolean, llvm::Type::getInt1Ty(*context)},
        {ast::prim_type::character, llvm::Type::getInt8Ty(*context)},
        // TODO: Move to a Rust style 2 ptr string instead of a C style null-terminated string
        {ast::prim_type::str, llvm::Type::getInt8PtrTy(*context)},
    } {}

llvm::Type * type_context::lower_to_llvm(ast::type_ptr type) {

    auto iter = std::find_if(active_types.begin(), active_types.end(),
                             [&type](const auto & entry) -> bool { return entry.first == type; });

    if (iter == active_types.end()) {
        if (type->is_pointer_type()) {
            // Find the pointed-to type.
            const auto & ast_ptr_type = dynamic_cast<const ast::ptr_type &>(*type);
            auto * pointed_to_type = lower_to_llvm(ast_ptr_type.pointed_to_type());
            assert(pointed_to_type != nullptr);
            return pointed_to_type->getPointerTo();
        }

        if (const auto * struct_type = dynamic_cast<const ast::struct_type *>(type.get());
            struct_type != nullptr) {

            std::vector<llvm::Type *> fields;
            for (auto i = 0U; i < struct_type->field_count(); ++i) {
                const auto & [name, type] = struct_type->field(i);
                fields.emplace_back(lower_to_llvm(type));
            }

            auto * llvm_struct_type = llvm::StructType::create(fields, struct_type->user_name());
            active_types.emplace(type, llvm_struct_type);
            return llvm_struct_type;
        }
        return nullptr;
    }
    return iter->second;
}
