#ifndef CODEGEN_HPP
#define CODEGEN_HPP

#include "ast/node_utils.hpp"
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
#define expand_node_macro(name) void visit(ast::name & name) override;
        ast_nodes
#undef expand_node_macro

        std::unique_ptr<llvm::Module> take_ir_module() && noexcept { return std::move(ir_module); }
        // clang-format on

        void verify_module() const;
        void dump() const;

      private:
        llvm::Type * find_type(const std::string & name,
                               std::optional<Location> loc = std::nullopt);
        [[nodiscard]] llvm::Value * find_alive_value(const std::string & name) const;

        void evaluate_short_circuit(ast::binary_expr &, llvm::Value * lhs_value);

        void syscall(ast::func_call_data &);

        void printError(const std::string & name, std::optional<Location> loc = std::nullopt) const;

        // Keep these behind unique_ptr to allow for moving the visitor
        std::unique_ptr<llvm::LLVMContext> context;
        std::unique_ptr<llvm::Module> ir_module;
        std::unique_ptr<llvm::IRBuilder<>> ir_builder;

        std::map<std::string, llvm::Type *> types;
        std::vector<std::map<std::string, llvm::Value *>> active_values;

        std::map<std::string, void (codegen::*)(ast::func_call_data &)> instrinics;
    };
} // namespace visitor

#endif
