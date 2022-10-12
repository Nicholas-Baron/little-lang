#include "./ast_to_cfg.hpp"

#include <catch2/catch.hpp>

TEST_CASE("ast_to_cfg is initially empty") {
    auto lowering = ast_to_cfg{};
    auto cfg = std::move(lowering).take_cfg();

    CHECK(cfg != nullptr);
    CHECK(cfg->previous_node() == nullptr);
}
