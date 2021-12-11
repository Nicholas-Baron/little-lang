#include <global_values.hpp>

void global_values::add(const std::string & mod, const std::string & id,
                        llvm::GlobalObject * value) {
    auto [mod_iter, _] = globals.emplace(mod, std::map<std::string, llvm::GlobalObject *>{});
    auto & module_exports = mod_iter->second;
    assert(module_exports.find(id) == module_exports.end());
    module_exports.emplace(id, value);
}

llvm::GlobalObject * global_values::lookup(const std::string & mod, const std::string & id) const {
    auto mod_iter = globals.find(mod);
    if (mod_iter == globals.end()) { return nullptr; }

    auto item_iter = mod_iter->second.find(id);
    return item_iter != mod_iter->second.end() ? item_iter->second : nullptr;
}
