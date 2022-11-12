#ifndef AST_SERIALIZER_HPP
#define AST_SERIALIZER_HPP

#include "visitor_base.hpp"

#include <iosfwd> // ostream

#include <move_copy.hpp>
#include <nlohmann/json.hpp>
#include <value_getter.hpp>

namespace ast {
    class serializer : public visitor_base,
                       public value_getter<serializer, ast::node, nlohmann::json> {
      public:
        explicit serializer(const std::string & filename);
        non_copyable(serializer);
        movable(serializer);
        ~serializer() noexcept override = default;

        void dump(std::ostream &, bool human_readable) const;

// clang-format off
#define expand_node_macro(name) void visit(name &) override;
        ast_nodes
#undef expand_node_macro
	  private:
            // clang-format on

            nlohmann::json mod;
    };
} // namespace ast

#endif
