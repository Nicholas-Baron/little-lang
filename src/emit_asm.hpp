#ifndef EMIT_ASM_HPP
#define EMIT_ASM_HPP

#include <llvm/IR/Module.h>

#include <memory>
#include <string>

[[nodiscard]] std::string init_llvm_targets();

[[nodiscard]] std::string make_output_name(const std::string &);

void emit_asm(std::unique_ptr<llvm::Module>, std::string && output_filename,
              bool debug_optimized_ir);

#endif
