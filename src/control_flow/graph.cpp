#include "graph.hpp"

#include "control_flow/serializer.hpp"
#include "node.hpp"

#include <iostream> // cout

namespace control_flow {
    graph::graph() = default;

    graph::~graph() noexcept = default;

    void graph::list_all_nodes() const noexcept { serializer::into_stream(std::cout, roots, true); }
} // namespace control_flow
