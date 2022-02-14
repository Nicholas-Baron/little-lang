#ifndef EMIT_ASM_HPP
#define EMIT_ASM_HPP

#include <memory>
#include <string>

#include <llvm/IR/Module.h>

[[nodiscard]] std::string init_llvm_targets();

void emit_asm(std::unique_ptr<llvm::Module> ir_module, std::string && output_filename,
              bool debug_optimized_ir);

#endif
