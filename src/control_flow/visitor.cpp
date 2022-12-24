#include "visitor.hpp"

#include "node.hpp"

namespace control_flow {
    void visitor::visit(control_flow::node & node) { node.accept(*this); }
} // namespace control_flow
