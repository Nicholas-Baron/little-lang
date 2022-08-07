#ifndef GLOBAL_VALUES_HPP
#define GLOBAL_VALUES_HPP

#include <map>
#include <ostream>
#include <string>

template<typename key_t, typename value_t>
class global_map final {
    static_assert(std::is_nothrow_constructible_v<value_t, std::nullptr_t>,
                  "global_map can only use pointer-like types for its value_t");

  public:
    [[nodiscard]] value_t lookup(const std::string & mod, const key_t & id) const {

        if constexpr (std::is_same_v<key_t, std::string>) { assert(not id.empty()); }

        auto mod_iter = globals.find(mod);
        if (mod_iter == globals.end()) { return nullptr; }

        auto item_iter = mod_iter->second.find(id);
        return item_iter != mod_iter->second.end() ? item_iter->second : nullptr;
    }

    void add(const std::string & mod, const key_t & id, value_t value) {

        if constexpr (std::is_same_v<key_t, std::string>) { assert(not id.empty()); }

        auto [mod_iter, _] = globals.emplace(mod, std::map<key_t, value_t>{});
        auto & module_exports = mod_iter->second;
        assert(module_exports.find(id) == module_exports.end());
        module_exports.emplace(id, value);
    }

  private:
    std::map<std::string, std::map<key_t, value_t>> globals;

    friend std::ostream & operator<<(std::ostream & lhs, const global_map<key_t, value_t> & rhs) {
        lhs << '{';
        for (auto & [mod_name, contents] : rhs.globals) {
            lhs << mod_name << " : {";
            for (auto & [name, item] : contents) { lhs << name << " : " << item << ", "; }
            lhs << "},";
        }
        return lhs << '}';
    }
};

#endif
