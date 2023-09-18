#include <control_flow/graph.hpp>
#include <intrinsics.hpp>

bool is_intrinsic(const std::string & mod, const std::string & id) {
    if (mod == "env") { return id == "arg_at" or id == "arg_count"; }
    return false;
}

control_flow::intrinsic_call * generate_ast_intrinsic(control_flow::graph & cfg, std::string name,
                                                      std::vector<control_flow::node *> args) {
    static const std::set<std::string> known_intrinsics{"syscall", "arg_at", "arg_count"};

    if (known_intrinsics.find(name) == known_intrinsics.end()) { return nullptr; }

    return &cfg.create<control_flow::intrinsic_call>(std::move(name), std::move(args));
}
