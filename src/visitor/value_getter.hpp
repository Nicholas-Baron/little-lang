#ifndef VALUE_GETTER_HPP
#define VALUE_GETTER_HPP

#include "visitor_base.hpp"
#include <type_traits>

namespace visitor {
    // NOTE: visitor_impl cannot be tested, as it is incomplete at the time of instantiation.
    template<typename visitor_impl, typename visitable, typename result_t>
    class value_getter {
      public:
        static_assert(std::is_class_v<visitable>, "we must visit classes");
        [[nodiscard]] static result_t get_value(visitable & n, visitor_impl & context) {
            n.accept(context);
            return context.result;
        }

        static_assert(not std::is_reference_v<result_t>, "we can only return non-references");
        void store_result(result_t value) { result = value; }

      private:
        result_t result;
    };

} // namespace visitor

#endif