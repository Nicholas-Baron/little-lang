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
    class typed_identifier final {
      public:
        typed_identifier(std::string && name, std::string && type, Location loc)
            : type_{std::move(type)}
            , name_{std::move(name)}
            , loc{loc} {}

        [[nodiscard]] const auto & name() const { return name_; }
        [[nodiscard]] const auto & type() const { return type_; }
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

        [[nodiscard]] const auto & name() const { return name_; }

        [[nodiscard]] size_t args_count() const { return args_.size(); }

        // TODO: stop returning const std::unique_ptr &
        [[nodiscard]] const auto & arg(size_t i) const {
            assert(i < args_.size());
            return args_.at(i);
        }

      private:
        std::string name_;
        std::vector<expr_ptr> args_{};
    };
} // namespace ast

#endif
