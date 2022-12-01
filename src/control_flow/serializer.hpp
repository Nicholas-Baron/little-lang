#ifndef CFG_SERIALIZER_HPP
#define CFG_SERIALIZER_HPP

#include "node_forward.hpp"
#include "utils/value_getter.hpp"
#include "visitor.hpp"

#include <nlohmann/json.hpp>

namespace control_flow {
    class serializer final : public visitor, public value_getter<serializer, node, nlohmann::json> {

      public:
        ~serializer() noexcept override = default;

        non_copyable(serializer);
        movable(serializer);

#define expand_node_macro(x) virtual void visit(x &) override;
        all_cfg_nodes
#undef expand_node_macro
    };
} // namespace control_flow

#endif
