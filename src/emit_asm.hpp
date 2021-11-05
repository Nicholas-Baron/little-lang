#ifndef EMIT_ASM_HPP
#define EMIT_ASM_HPP

#include "context_module.hpp"

#include <string>

[[nodiscard]] std::string init_llvm_targets();

[[nodiscard]] std::string make_output_name(const std::string &);

void emit_asm(context_module && context, std::string && target_triple,
              std::string && output_filename);

#endif
