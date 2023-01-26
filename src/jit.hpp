#pragma once

#include <memory> // unique_ptr

#include <llvm/IR/Module.h>

[[nodiscard]] uint64_t run_module(std::vector<std::unique_ptr<llvm::Module>> modules);
