#pragma once

#include <cassert>
#include <map>
#include <optional> // nullopt_t
#include <ostream>
#include <string>
#include <variant> // monostate

template<typename dest_t, typename... src_t>
static constexpr bool can_nothrow_construct_any
    = (false or ... or std::is_nothrow_constructible_v<dest_t, src_t>);

template<typename key_t, typename value_t>
class global_map final {
    static_assert(
        can_nothrow_construct_any<value_t, std::nullptr_t, std::nullopt_t, std::monostate>,
        "global_map can only use possibly empty types for `value_t`");

  public:
    [[nodiscard]] value_t lookup(const std::string & mod, const key_t & key) const {

        if constexpr (std::is_same_v<key_t, std::string>) { assert(not key.empty()); }

        auto mod_iter = globals.find(mod);
        if (mod_iter == globals.end()) { return empty_value(); }

        auto item_iter = mod_iter->second.find(key);
        return item_iter != mod_iter->second.end() ? item_iter->second : empty_value();
    }

    void add(const std::string & mod, const key_t & key, value_t value) {

        if constexpr (std::is_same_v<key_t, std::string>) { assert(not key.empty()); }

        auto [mod_iter, _] = globals.emplace(mod, std::map<key_t, value_t>{});
        auto & module_exports = mod_iter->second;
        assert(module_exports.find(key) == module_exports.end());
        module_exports.emplace(key, value);
    }

    [[nodiscard]] static constexpr value_t empty_value() {
        if constexpr (std::is_nothrow_constructible_v<value_t, std::nullptr_t>) {
            return nullptr;
        } else if constexpr (std::is_nothrow_constructible_v<value_t, std::nullopt_t>) {
            return std::nullopt;
        } else if constexpr (std::is_nothrow_constructible_v<value_t, std::monostate>) {
            return std::monostate{};
        }
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
