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
        static nlohmann::json to_json(std::string filename, ast::top_level_sequence & mod) {
            ast::serializer serializer;
            serializer.visit(mod);
            return {{"filename", std::move(filename)}, {"contents", serializer.get_result()}};
        }

        static void into_stream(std::ostream & stream, std::string filename,
                                ast::top_level_sequence & mod, bool human_readable) {
            ast::serializer serializer;
            serializer.visit(mod);

            stream << nlohmann::json{{"filename", std::move(filename)},
                                     {"contents", serializer.get_result()}}
                          .dump(human_readable ? 4 : -1);
        }

        non_copyable(serializer);
        movable(serializer);
        ~serializer() noexcept override = default;

      private:
        serializer() = default;
// clang-format off
#define expand_node_macro(name) void visit(name &) override;
        ast_nodes
#undef expand_node_macro
        // clang-format on
    };
} // namespace ast

#endif
