#ifndef CODEGEN_HPP
#define CODEGEN_HPP

#include "location.hpp"
#include "value_getter.hpp"
#include "visitor_base.hpp"
#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

#include <optional>

namespace visitor {
    class codegen final : public visitor_base,
                          public value_getter<codegen, ast::node, llvm::Value *> {
      public:
        explicit codegen(const std::string & name);

        non_copyable(codegen);

        movable(codegen);

        ~codegen() override = default;

        // clang-format off
#define __node(name) void visit(ast::name & name) override;
        ast_nodes
#undef __node

        std::unique_ptr<llvm::Module> take_module() && noexcept { return std::move(ir_module); }
        // clang-format on

        void verify_module() const;
        void dump() const;

      private:
        void printError(const std::string & name, std::optional<Location> loc = std::nullopt);

        // Keep these behind unique_ptr to allow for moving the visitor
        std::unique_ptr<llvm::LLVMContext> context;
        std::unique_ptr<llvm::Module> ir_module;
        std::unique_ptr<llvm::IRBuilder<>> ir_builder;
    };
} // namespace visitor

#endif
