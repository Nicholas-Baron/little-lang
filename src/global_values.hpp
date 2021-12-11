#ifndef GLOBAL_VALUES_HPP
#define GLOBAL_VALUES_HPP

#include <llvm/IR/GlobalObject.h>

#include <map>
#include <string>

class global_values final {
  public:
    [[nodiscard]] llvm::GlobalObject * lookup(const std::string & mod,
                                              const std::string & id) const;

    void add(const std::string & mod, const std::string & id, llvm::GlobalObject *);

  private:
    std::map<std::string, std::map<std::string, llvm::GlobalObject *>> globals;
};

#endif
