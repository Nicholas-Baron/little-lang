#pragma once

#include "node_forward.hpp"
#include "utils/value_getter.hpp"
#include "visitor.hpp"

namespace control_flow {
    /// The value returned is actually an index into the result vector.
    class serializer final
        : public visitor
        , public value_getter<serializer, node, size_t> {
      public:
        static void into_stream(std::ostream & stream, const std::vector<node *> & roots);

        ~serializer() noexcept override;

        non_copyable(serializer);
        movable(serializer);

      private:
        // clang-format off
#define expand_node_macro(x) virtual void visit(x &) override;
        all_cfg_nodes
#undef expand_node_macro

        serializer();

        // clang-format on

        struct node_data {
            node * ptr = nullptr;
            std::string name;
            std::map<size_t, std::string> connections;

            [[nodiscard]] std::string id() const;
        };

        void store_result(node_data * value);

        std::pair<node_data *, size_t> add_node(node * node);

        std::vector<std::unique_ptr<node_data>> node_array;
        std::map<node *, size_t> visited;
    };
} // namespace control_flow
