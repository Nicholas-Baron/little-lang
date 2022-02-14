#ifndef GLOBAL_VALUES_HPP
#define GLOBAL_VALUES_HPP

#include <map>
#include <string>

#include <llvm/IR/GlobalObject.h>

template<typename key_t, typename value_t>
class global_map final {
  public:
    [[nodiscard]] value_t lookup(const std::string & mod, const key_t & id) const {
        auto mod_iter = globals.find(mod);
        if (mod_iter == globals.end()) { return nullptr; }

        auto item_iter = mod_iter->second.find(id);
        return item_iter != mod_iter->second.end() ? item_iter->second : nullptr;
    }

    void add(const std::string & mod, const key_t & id, value_t value) {

        auto [mod_iter, _] = globals.emplace(mod, std::map<std::string, value_t>{});
        auto & module_exports = mod_iter->second;
        assert(module_exports.find(id) == module_exports.end());
        module_exports.emplace(id, value);
    }

  private:
    std::map<std::string, std::map<key_t, value_t>> globals;
};

#endif
