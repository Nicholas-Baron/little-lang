#ifndef NODE_UTILS_HPP
#define NODE_UTILS_HPP

#include "base_nodes.hpp"
#include "context_module.hpp"
#include "location.hpp"
#include "utils/move_copy.hpp"

#include <string>

// Classes that are utilities to ast nodes,
// but not nodes themselves

namespace ast {
    class Typed_Var final {
      public:
        Typed_Var(std::string && name, std::string && type)
            : type_{std::move(type)}
            , name_{std::move(name)} {}

        [[nodiscard]] const auto & name() const { return name_; }
        [[nodiscard]] const auto & type() const { return type_; }

        void set_location(const Location & loc_new) { loc = loc_new; }

        [[nodiscard]] const auto & location() const noexcept { return loc; }

      private:
        std::string type_;
        std::string name_;
        Location loc{};
    };

    // Stores the function call data.
    // is facaded by func_call_expr and func_call_stmt
    class func_call_data final {
      public:
        func_call_data(std::string && name, std::vector<expr_ptr> && args)
            : name_(std::move(name))
            , args_{std::move(args)} {}

        non_copyable(func_call_data);

        movable(func_call_data);

        llvm::Value * codegen(context_module & context);

        llvm::Type * type_check(context_module &, Location);

      private:
        std::string name_;
        std::vector<expr_ptr> args_{};
    };
} // namespace ast

#endif
