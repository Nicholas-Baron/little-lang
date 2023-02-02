#pragma once

#include <memory>
#include <string>

#include <llvm/IR/Module.h>

[[nodiscard]] std::string init_llvm_targets();

// Returns `true` if the module was successfully optimized
bool optimize_module(llvm::Module & ir_module, bool debug_optimized_ir);

void emit_asm(std::unique_ptr<llvm::Module> ir_module, std::string && output_filename);
