#ifndef printer_HPP
#define printer_HPP

#include "location.hpp"
#include "value_getter.hpp"
#include "visitor_base.hpp"

#include <optional>

#include <llvm/IR/IRBuilder.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Value.h>

namespace visitor {
    class printer final : public visitor_base,
                          public value_getter<printer, ast::node, llvm::Value *> {
      public:
        explicit printer(const std::string & name);

        non_copyable(printer);

        movable(printer);

        ~printer() override = default;

        // clang-format off
#define expand_node_macro(name) void visit(ast::name & name) override;
        ast_nodes
#undef expand_node_macro

        void verify_module() const;

        void dump() const;
        // clang-format on

      private:
        void printError(const std::string & name, std::optional<Location> loc = std::nullopt);
    };
} // namespace visitor

#endif
