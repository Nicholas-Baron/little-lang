#ifndef NODE_UTILS_HPP
#define NODE_UTILS_HPP

#include "base_nodes.hpp"
#include "location.hpp"
#include "utils/move_copy.hpp"

#include <cassert>
#include <string>
#include <vector>

// Classes that are utilities to ast nodes,
// but not nodes themselves

namespace ast {
    class type final {
      public:
        explicit type(std::string && type)
            : base_type_{std::move(type)} {}

        [[nodiscard]] const auto & base_type() const { return base_type_; }

      private:
        friend bool operator==(const type & lhs, const type & rhs) {
            return lhs.base_type_ == rhs.base_type_;
        }

        friend bool operator<(const type & lhs, const type & rhs) {
            return lhs.base_type_ < rhs.base_type_;
        }

        std::string base_type_;
    };

    class typed_identifier final {
      public:
        typed_identifier(std::string && name, ast::type && type, Location loc)
            : type_{std::move(type)}
            , name_{std::move(name)}
            , loc{loc} {}

        [[nodiscard]] const auto & name() const { return name_; }
        [[nodiscard]] const auto & type() const { return type_; }
        [[nodiscard]] const auto & location() const noexcept { return loc; }

      private:
        ast::type type_;
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

        [[nodiscard]] const auto & name() const { return name_; }

        [[nodiscard]] size_t args_count() const { return args_.size(); }

        [[nodiscard]] ast::expr & arg(size_t i) const {
            assert(i < args_.size());
            assert(args_.at(i) != nullptr);
            return *args_.at(i);
        }

      private:
        std::string name_;
        std::vector<expr_ptr> args_{};
    };
} // namespace ast

#endif
