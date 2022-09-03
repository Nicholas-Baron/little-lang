#ifndef VALUE_GETTER_HPP
#define VALUE_GETTER_HPP

#include "ast/visitor_base.hpp"

#include <cassert>
#include <optional>
#include <type_traits>

namespace visitor {
    // Invariant 1: `value_getter` will *never* implicitly drop a result.
    // NOTE: visitor_impl cannot be tested, as it is incomplete at the time of instantiation.
    template<typename visitor_impl, typename visitable, typename result_t>
    class value_getter {
      public:
        static_assert(std::is_class_v<visitable>, "we must visit classes");
        [[nodiscard]] static result_t get_value(visitable & n, visitor_impl & context) {
            assert(not context.result.has_value());
            n.accept(context);
            return context.get_result();
        }

        static_assert(not std::is_reference_v<result_t>, "we can only return non-references");
        void store_result(result_t value) {
            assert(not result.has_value());
            result = value;
        }

        // This function returns the result previously stored.
        // Only call this if `get_value` cannot be used.
        [[nodiscard]] result_t get_result() {
            assert(result.has_value());
            auto value = std::move(result).value();
            result.reset();
            assert(not result.has_value());
            return value;
        }

        // This function drops the result that was previously stored.
        // Only call this if the result is meant to be ignored.
        void drop_result() {
            assert(result.has_value());
            result.reset();
        }

      private:
        std::optional<result_t> result;
    };

} // namespace visitor

#endif
