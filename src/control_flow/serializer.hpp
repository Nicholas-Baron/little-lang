#ifndef CFG_SERIALIZER_HPP
#define CFG_SERIALIZER_HPP

#include "node_forward.hpp"
#include "utils/value_getter.hpp"
#include "visitor.hpp"

#include <nlohmann/json_fwd.hpp>

namespace control_flow {
    /// The value returned is actually an index into the result vector.
    class serializer final : public visitor, public value_getter<serializer, node, size_t> {
      public:
        static void into_stream(std::ostream &, const std::vector<node *> &, bool human_readable);

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

        void store_result(nlohmann::json *);

        std::pair<nlohmann::json *, size_t> add_node(node *);

        std::vector<std::unique_ptr<nlohmann::json>> graph_array;
        std::map<node *, size_t> visited;
    };
} // namespace control_flow

#endif
