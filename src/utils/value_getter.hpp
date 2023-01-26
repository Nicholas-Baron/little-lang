#pragma once

#include <cassert>
#include <optional>
#include <type_traits>

/// This class is used to return values in visitors.
/// Usage:
/// ```
/// class my_visitor : public visitor_base,
///                    public value_getter<my_visitor, visited_type, result_type> {};
/// ```

// Invariant 1: `value_getter` will *never* implicitly drop a result.
// NOTE: visitor_t cannot be tested, as it is incomplete at the time of instantiation.
template<typename visitor_t, typename visitable_t, typename result_t>
class value_getter {
  public:
    static_assert(std::is_class_v<visitable_t>, "we must visit classes");
    [[nodiscard]] static result_t get_value(visitable_t & node, visitor_t & context) {
        assert(not context.result.has_value());
        node.accept(context);
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
