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

      private:
        std::vector<std::unique_ptr<node>> nodes{};
    };
} // namespace control_flow

#endif
