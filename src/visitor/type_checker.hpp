#ifndef type_checker_HPP
#define type_checker_HPP

#include "ast/node_utils.hpp"
#include "ast/type.hpp"
#include "global_map.hpp"
#include "location.hpp"
#include "value_getter.hpp"
#include "visitor_base.hpp"

#include <map>
#include <optional>
#include <vector>

#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Type.h>

namespace visitor {
    class type_checker final : public visitor_base,
                               public value_getter<type_checker, ast::node, ast::type_ptr> {
      public:
        type_checker(std::string filename, llvm::LLVMContext & context,
                     global_map<std::string, ast::type_ptr> & globals);

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
        void syscall(ast::func_call_data & func_call_data);

        ast::type_ptr evaluate_arithmetic(ast::type_ptr && lhs, ast::type_ptr && rhs);

        ast::type_ptr evaluate_comparison(ast::type_ptr && lhs, ast::type_ptr && rhs);

        template<class... arg_t>
        void printError(std::optional<Location> loc, const arg_t &... args);

        // Find the type of an identifier
        [[nodiscard]] ast::type_ptr find_type_of(const std::string & id) const;

        void bind_type(ast::type_ptr type, std::string identifier, bool should_export = false);

        // Stores the `type_ptr` into both the value_getter and the `ast::expr &`
        void store_result(const ast::type_ptr & type, ast::expr & expr) {
            value_getter::store_result(type);
            expr.type = type;
        }

        bool found_error{false};

        std::string filename;
        llvm::LLVMContext & context;

        std::vector<std::map<std::string, ast::type_ptr>> active_typed_identifiers;
        global_map<std::string, ast::type_ptr> & program_globals;
        std::map<std::string, void (type_checker::*)(ast::func_call_data &)> instrinics;

        const ast::type * current_return_type{nullptr};
        const std::string * current_function_name{nullptr};
    };
} // namespace visitor

#endif
