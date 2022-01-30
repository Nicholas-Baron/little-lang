#ifndef type_checker_HPP
#define type_checker_HPP

#include "ast/type.hpp"
#include "global_map.hpp"
#include "location.hpp"
#include "type_context.hpp"
#include "value_getter.hpp"
#include "visitor_base.hpp"
#include <ast/node_utils.hpp>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

#include <map>
#include <optional>
#include <vector>

namespace visitor {
    class type_checker final : public visitor_base,
                               public value_getter<type_checker, ast::node, ast::type_ptr> {
      public:
        type_checker(std::string filename, llvm::LLVMContext &,
                     global_map<std::string, ast::type_ptr> &, type_context &);

        non_copyable(type_checker);

        non_movable(type_checker);

        ~type_checker() override = default;

        // clang-format off
#define expand_node_macro(name) void visit(ast::name & name) override;
        ast_nodes
#undef expand_node_macro

		[[nodiscard]] bool checked_good() const { return not found_error; }
        // clang-format on

      private:
        void syscall(ast::func_call_data &);

        void evaluate_arithmetic(ast::type_ptr && lhs, ast::type_ptr && rhs);
        void evaluate_comparison(ast::binary_expr & expr, ast::type_ptr && lhs,
                                 ast::type_ptr && rhs);

        // TODO: Actually implement this
        void printError(const std::string & name, std::optional<Location> loc = std::nullopt);

        // Find the type of an identifier
        [[nodiscard]] ast::type_ptr find_type_of(const std::string &) const;

        void bind_type(ast::type_ptr, std::string, bool should_export = false);

        bool found_error{false};

        std::string filename;
        llvm::LLVMContext & context;

        type_context & type_context;
        std::vector<std::map<std::string, ast::type_ptr>> active_typed_identifiers;
        global_map<std::string, ast::type_ptr> & program_globals;
        std::map<std::string, void (type_checker::*)(ast::func_call_data &)> instrinics;

        const ast::type * current_return_type{nullptr};
    };
} // namespace visitor

#endif
