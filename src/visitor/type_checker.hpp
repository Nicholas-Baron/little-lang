#ifndef type_checker_HPP
#define type_checker_HPP

#include "location.hpp"
#include "value_getter.hpp"
#include "visitor_base.hpp"
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

#include <map>
#include <optional>
#include <vector>

namespace visitor {
    class type_checker final : public visitor_base,
                               public value_getter<type_checker, ast::node, llvm::Type *> {
      public:
        type_checker(std::string filename, llvm::LLVMContext *,
                     std::map<std::string, std::map<std::string, llvm::Type *>> *);

        non_copyable(type_checker);

        movable(type_checker);

        ~type_checker() override = default;

        // clang-format off
#define expand_node_macro(name) void visit(ast::name & name) override;
        ast_nodes
#undef expand_node_macro

		[[nodiscard]] bool checked_good() const { return not found_error; }
        // clang-format on

      private:
        void syscall(ast::func_call_data &);

        // TODO: Actually implement this
        void printError(const std::string & name, std::optional<Location> loc = std::nullopt);

        [[nodiscard]] llvm::Type * find_type_of(const std::string &) const;
        void bind_type(llvm::Type *, std::string, bool should_export = false);

        bool found_error{false};

        std::string filename;
        llvm::LLVMContext * context;

        std::vector<std::map<std::string, llvm::Type *>> active_typed_identifiers;
        std::map<std::string, std::map<std::string, llvm::Type *>> * program_globals;
        std::map<std::string, void (type_checker::*)(ast::func_call_data &)> instrinics;

        llvm::Type * current_return_type{nullptr};
    };
} // namespace visitor

#endif
