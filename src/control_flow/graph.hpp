#pragma once

#include <memory> // unique_ptr
#include <vector>

#include <move_copy.hpp>

namespace control_flow {
    class node;

    // Invariant: No function can call out of the graph.
    class graph final {
      public:
        template<typename T, typename... Args>
        [[nodiscard]] T & create(Args... args) {
            auto owner = std::make_unique<T>(args...);
            auto & ref = *owner;
            nodes.emplace_back(std::move(owner));
            previously_created = &ref;
            return ref;
        }

        template<typename T, typename... Args>
        [[nodiscard]] T & create_root(Args... args) {
            auto owner = std::make_unique<T>(args...);
            auto & ref = *owner;
            roots.emplace_back(owner.get());
            nodes.emplace_back(std::move(owner));
            previously_created = &ref;
            return ref;
        }

        graph();

        non_copyable(graph);
        movable(graph);

        ~graph() noexcept;

        [[nodiscard]] node * previous_node() const noexcept {
            assert(previously_created != nullptr);
            return previously_created;
        }

        template<typename Callable>
        void for_each_function(Callable func) const noexcept {
            for (const auto * function : roots) { func(function); }
        }

        template<typename Callable>
        void for_each_function(Callable func) noexcept {
            for (auto * function : roots) { func(function); }
        }

        template<typename Callable>
        void for_each_node(Callable func) const noexcept {
            for (const auto & node : nodes) { func(node.get()); }
        }

        template<typename Callable>
        void for_each_node(Callable func) noexcept {
            for (auto & node : nodes) { func(node.get()); }
        }

        void list_all_nodes() const noexcept;

        [[nodiscard]] const std::string & program_name() const { return prog_name; }

        void set_program_name(std::string name) { prog_name = std::move(name); }

      private:
        std::vector<std::unique_ptr<node>> nodes{};

        // Invariant: all pointers in roots must have an equivalent in nodes.
        std::vector<node *> roots{};

        node * previously_created{nullptr};

        std::string prog_name;
    };
} // namespace control_flow
