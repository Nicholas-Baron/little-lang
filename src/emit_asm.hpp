#ifndef EMIT_ASM_HPP
#define EMIT_ASM_HPP

#include "context_module.hpp"

#include <string>

[[nodiscard]] std::string init_llvm_targets();

void emit_asm(context_module && context, std::string && target_triple);

#endif
