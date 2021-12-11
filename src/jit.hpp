#ifndef JIT_HPP
#define JIT_HPP

#include <llvm/IR/Module.h>

#include <memory> // unique_ptr

[[nodiscard]] int run_module(std::vector<std::unique_ptr<llvm::Module>>);

#endif
