#ifndef JIT_HPP
#define JIT_HPP

#include <memory> // unique_ptr

#include <llvm/IR/Module.h>

[[nodiscard]] uint64_t run_module(std::vector<std::unique_ptr<llvm::Module>> modules);

#endif
