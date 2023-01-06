#include "ast/nodes.hpp"
#include "ast/parser.hpp"
#include "ast_to_cfg.hpp"
#include "control_flow/node.hpp"

#include <iostream>
#include <queue>

#include <catch2/catch.hpp>

TEST_CASE("ast_to_cfg is initially empty") {
    auto lowering = ast_to_cfg{};
    auto cfg = std::move(lowering).take_cfg();

    CHECK(cfg != nullptr);
}

static void scan_graph(const control_flow::function_start * start,
                       const control_flow::function_end * func_end) {

    std::queue<const control_flow::node *> frontier;
    frontier.push(start->next);
    auto found_end = false;

    while (not frontier.empty()) {
        const auto * visiting = frontier.front();
        CHECK(visiting != nullptr);

        std::cout << "Visiting " << typeid(*visiting).name() << " @ "
                  << static_cast<const void *>(visiting) << std::endl;

        if (visiting == func_end) {
            found_end = true;
        } else if (const auto * bin_op
                   = dynamic_cast<const control_flow::binary_operation *>(visiting);
                   bin_op != nullptr) {
            CHECK(bin_op->next != nullptr);
            frontier.push(bin_op->next);
        } else if (const auto * branch = dynamic_cast<const control_flow::branch *>(visiting);
                   branch != nullptr) {
            frontier.push(branch->true_case);
            frontier.push(branch->false_case);
        } else if (const auto * value = dynamic_cast<const control_flow::constant *>(visiting);
                   value != nullptr) {
            CHECK(value->next != nullptr);
            frontier.push(value->next);
        } else if (const auto * phi = dynamic_cast<const control_flow::phi *>(visiting);
                   phi != nullptr) {
            CHECK(phi->next != nullptr);
            frontier.push(phi->next);
        } else {
            std::cout << typeid(*visiting).name() << std::endl;
            assert(false);
        }

        frontier.pop();
    }

    CHECK(found_end);
}

TEST_CASE("ast_to_cfg can lower an empty function") {
    std::string buffer = "main() {}";
    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);
    auto mod = parser->parse();

    auto lowering = ast_to_cfg{};

    lowering.visit(*mod);

    auto cfg = std::move(lowering).take_cfg();
    CHECK(cfg != nullptr);

    // There should be 1 root.
    const control_flow::function_start * start = nullptr;
    auto root_count = 0U;
    cfg->for_each_function([&](auto * root) {
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

TEST_CASE("ast_to_cfg can lower a shortcircuiting expression") {
    std::string buffer = "positive_even(x : int) = x > 0 and x % 2 == 0";
    std::cout << "Testing " << buffer << std::endl;

    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);
    auto mod = parser->parse();

    auto lowering = ast_to_cfg{};

    lowering.visit(*mod);

    auto cfg = std::move(lowering).take_cfg();
    CHECK(cfg != nullptr);

    // There should be 1 root.
    const control_flow::function_start * start = nullptr;
    auto root_count = 0U;
    cfg->for_each_function([&](auto * root) {
        CHECK(root != nullptr);
        ++root_count;

        const auto * func_start = dynamic_cast<const control_flow::function_start *>(root);
        CHECK(func_start != nullptr);
        start = func_start;
        CHECK(func_start->arg_count == 1);

        CHECK_FALSE(func_start->exported);
        CHECK(func_start->next != nullptr);
    });
    CHECK(root_count == 1);

    // The last node generated should be a function end.
    CHECK(cfg->previous_node() != nullptr);
    auto * func_end = dynamic_cast<control_flow::function_end *>(cfg->previous_node());
    CHECK(func_end != nullptr);

    scan_graph(start, func_end);
}
