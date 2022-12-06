#ifndef CFG_SERIALIZER_HPP
#define CFG_SERIALIZER_HPP

#include "node_forward.hpp"
#include "utils/value_getter.hpp"
#include "visitor.hpp"

#include <nlohmann/json.hpp>

namespace control_flow {
    class serializer final : public visitor, public value_getter<serializer, node, nlohmann::json> {
      public:
        static void into_stream(std::ostream &, const std::vector<node *> &, bool human_readable);

        ~serializer() noexcept override = default;

        non_copyable(serializer);
        movable(serializer);

        // clang-format off
#define expand_node_macro(x) virtual void visit(x &) override;
        all_cfg_nodes
#undef expand_node_macro

      private:
        serializer() = default;
        // clang-format on
    };
} // namespace control_flow

#endif
