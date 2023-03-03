#pragma once

#include "control_flow/node.hpp"

[[nodiscard]] bool is_intrinsic(const std::string & mod, const std::string & id);

control_flow::intrinsic_call * generate_ast_intrinsic(control_flow::graph & cfg, std::string name,
                                                      std::vector<control_flow::node *> args);
