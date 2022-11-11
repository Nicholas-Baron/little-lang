#ifndef AST_SERIALIZER_HPP
#define AST_SERIALIZER_HPP

#include "visitor_base.hpp"

#include <move_copy.hpp>
#include <nlohmann/json.hpp>
#include <value_getter.hpp>

namespace ast {
    class serializer : public visitor_base,
                       public value_getter<serializer, ast::node, nlohmann::json> {
      public:
        non_copyable(serializer);
        movable(serializer);
        ~serializer() noexcept override = default;

      private:
    };
} // namespace ast

#endif
