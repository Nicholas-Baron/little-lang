#include "ast/nodes.hpp"
#include "ast/parser.hpp"
#include "ast_to_cfg.hpp"

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
    CHECK(cfg->previous_node() != nullptr);
}
