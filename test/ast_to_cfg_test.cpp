#include "ast/nodes.hpp"
#include "ast/parser.hpp"
#include "ast_to_cfg.hpp"
#include "control_flow/node.hpp"

#include <catch2/catch.hpp>

TEST_CASE("ast_to_cfg is initially empty") {
    auto lowering = ast_to_cfg{};
    auto cfg = std::move(lowering).take_cfg();

    CHECK(cfg != nullptr);
    CHECK(cfg->previous_node() == nullptr);
}

TEST_CASE("ast_to_cfg can lower an empty function") {
    std::string buffer = "main() {}";
    auto parser = parser::from_buffer(buffer);
    auto mod = parser->parse();

    auto lowering = ast_to_cfg{};

    lowering.visit(*mod);

    auto cfg = std::move(lowering).take_cfg();
    CHECK(cfg != nullptr);

    // There should be 1 root.
    const control_flow::function_start * start = nullptr;
    auto root_count = 0U;
    cfg->for_each_root([&](auto * root) {
        CHECK(root != nullptr);
        ++root_count;

        const auto * func_start = dynamic_cast<const control_flow::function_start *>(root);
        CHECK(func_start != nullptr);
        start = func_start;
        CHECK(func_start->arg_count == 0);

        // Note that even though LLVM requires main to be exported,
        // at the language level, it is not.
        CHECK_FALSE(func_start->exported);
        CHECK(func_start->next != nullptr);
    });
    CHECK(root_count == 1);

    // The last node generated should be a function end.
    CHECK(cfg->previous_node() != nullptr);
    auto * func_end = dynamic_cast<control_flow::function_end *>(cfg->previous_node());
    CHECK(func_end != nullptr);
    CHECK(start->next == func_end);
    CHECK(func_end->previous == start);
}