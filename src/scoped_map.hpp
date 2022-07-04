#ifndef SCOPED_MAP_HPP
#define SCOPED_MAP_HPP

#include <map>
#include <vector>

template<typename key_t, typename value_t>
class scoped_map final {
  public:
    scoped_map()
        : active_values{{}} {}

    // Iterate thru the scopes in reverse
    auto begin() noexcept { return active_values.rbegin(); }
    auto end() noexcept { return active_values.rend(); }

    auto begin() const noexcept { return active_values.rbegin(); }
    auto end() const noexcept { return active_values.rend(); }

    auto add_to_root(key_t key, value_t value) {
        return active_values.front().emplace(std::move(key), std::move(value));
    }

    auto add_to_current_scope(key_t key, value_t value) {
        return active_values.back().emplace(std::move(key), std::move(value));
    }

    auto & add_scope() { return active_values.emplace_back(); }

    void remove_scope() { active_values.pop_back(); }

  private:
    std::vector<std::map<key_t, value_t>> active_values;
};

#endif
