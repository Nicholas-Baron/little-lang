#include "graph.hpp"

#include "node.hpp"

#include <iomanip>  // hex
#include <iostream> // cout

namespace control_flow {
    graph::graph() = default;

    graph::~graph() noexcept = default;

    void graph::list_all_nodes() const noexcept {
        std::cout << std::hex;
        for_each_node([](auto * node) { std::cout << static_cast<void *>(node) << '\n'; });
    }
} // namespace control_flow
