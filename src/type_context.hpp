#ifndef TYPE_CONTEXT_HPP
#define TYPE_CONTEXT_HPP

#include "ast/node_utils.hpp"
#include "global_map.hpp"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

#include <map>

class type_context final {
  public:
    explicit type_context(llvm::LLVMContext *);

    [[nodiscard]] llvm::Type * lower_to_llvm(const ast::type &);

  private:
    global_map<std::string, ast::type_ptr> global_types;
    std::map<ast::type_ptr, llvm::Type *> active_types;
};

#endif
