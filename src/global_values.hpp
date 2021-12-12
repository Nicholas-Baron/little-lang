#ifndef GLOBAL_VALUES_HPP
#define GLOBAL_VALUES_HPP

#include <llvm/IR/GlobalObject.h>

#include <map>
#include <string>

template<typename value_t>
class global_map final {
  public:
    [[nodiscard]] value_t lookup(const std::string & mod, const std::string & id) const {
        auto mod_iter = globals.find(mod);
        if (mod_iter == globals.end()) { return nullptr; }

        auto item_iter = mod_iter->second.find(id);
        return item_iter != mod_iter->second.end() ? item_iter->second : nullptr;
    }

    void add(const std::string & mod, const std::string & id, value_t value) {

        auto [mod_iter, _] = globals.emplace(mod, std::map<std::string, value_t>{});
        auto & module_exports = mod_iter->second;
        assert(module_exports.find(id) == module_exports.end());
        module_exports.emplace(id, value);
    }

  private:
    std::map<std::string, std::map<std::string, value_t>> globals;
};

#endif
