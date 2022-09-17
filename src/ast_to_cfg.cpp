#include "ast_to_cfg.hpp"

#include "control_flow/node.hpp"

ast_to_cfg::ast_to_cfg()
    : result_cfg{std::make_unique<control_flow::graph>()} {}
