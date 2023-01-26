#pragma once

#include "ast/node_utils.hpp"
#include "ast/type_context.hpp"
#include "utils/global_map.hpp"

#include <map>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

class llvm_type_lowering final {
  public:
    llvm_type_lowering(ast::type_context & type_context, llvm::LLVMContext * context);

    [[nodiscard]] llvm::Type * lower_to_llvm(ast::type_ptr type);

  private:
    global_map<std::string, ast::type_ptr> global_types;
    std::map<ast::type_ptr, llvm::Type *> active_types;
};
