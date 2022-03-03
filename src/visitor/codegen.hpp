#ifndef CODEGEN_HPP
#define CODEGEN_HPP

#include "ast/node_utils.hpp"
#include "global_map.hpp"
#include "location.hpp"
#include "type_context.hpp"
#include "value_getter.hpp"
#include "visitor_base.hpp"

#include <optional>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>

namespace visitor {
    class codegen final : public visitor_base,
                          public value_getter<codegen, ast::node, llvm::Value *> {
      public:
        codegen(const std::string & name, llvm::LLVMContext & context,
                global_map<std::string, llvm::GlobalObject *> & globals,
                type_context & typ_context);

        non_copyable(codegen);

        non_movable(codegen);

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
        llvm::Type * find_type(const ast::type & ast_type,
                               std::optional<Location> loc = std::nullopt);

        [[nodiscard]] llvm::Value * find_alive_value(const std::string & name,
                                                     bool should_error = true) const;

        void evaluate_comparison(ast::binary_expr & expr, llvm::Value * lhs_value,
                                 llvm::Value * rhs_value, bool is_float, bool is_constant);

        void evaluate_pointer_math(ast::binary_expr & expr, llvm::Value * lhs_value,
                                   llvm::Value * rhs_value);

        void evaluate_short_circuit(ast::binary_expr & binary_expr, llvm::Value * lhs_value);

        void arg_count(ast::func_call_data & /*unused*/);
        void arg_at(ast::func_call_data & /*data*/);
        void syscall(ast::func_call_data & /*func_call_data*/);

        void printError(const std::string & name, std::optional<Location> loc = std::nullopt) const;

        // Keep these behind unique_ptr to allow for moving the visitor
        // context is not owned by us, but is sent to us via the constructor.
        llvm::LLVMContext & context;
        std::unique_ptr<llvm::Module> ir_module;
        std::unique_ptr<llvm::IRBuilder<>> ir_builder;

        type_context & type_context;
        std::vector<std::map<std::string, llvm::Value *>> active_values;
        global_map<std::string, llvm::GlobalObject *> & program_globals;

        std::map<std::string, void (codegen::*)(ast::func_call_data &)> instrinics;
    };
} // namespace visitor

#endif
