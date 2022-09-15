#ifndef GRAPH_HPP
#define GRAPH_HPP

#include <memory> // unique_ptr
#include <vector>

#include <move_copy.hpp>

namespace control_flow {
    class node;

    class graph final {
      public:
        template<typename T, typename... Args>
        [[nodiscard]] T & create(Args... args) {
            auto owner = std::make_unique<T>(args...);
            auto & ref = *owner;
            nodes.emplace_back(std::move(owner));
            return ref;
        }

        template<typename T, typename... Args>
        [[nodiscard]] T & create_root(Args... args) {
            auto owner = std::make_unique<T>(args...);
            auto & ref = *owner;
            roots.emplace_back(owner.get());
            nodes.emplace_back(std::move(owner));
            return ref;
        }

        graph();

        non_copyable(graph);
        movable(graph);

        ~graph() noexcept;

      private:
        std::vector<std::unique_ptr<node>> nodes{};

        // Invariant: all pointers in roots must have an equivalent in nodes.
        std::vector<node *> roots{};
    };
} // namespace control_flow

#endif
