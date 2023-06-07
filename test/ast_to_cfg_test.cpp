#include "ast/nodes.hpp"
#include "ast/parser.hpp"
#include "ast_to_cfg.hpp"
#include "control_flow/node.hpp"

#include <iostream>
#include <queue>

#include <catch2/catch.hpp>

TEST_CASE("ast_to_cfg is initially empty") {
    auto type_context = ast::type_context{};
    auto lowering = ast_to_cfg{type_context};
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
        } else if (const auto * intrinsic_call
                   = dynamic_cast<const control_flow::intrinsic_call *>(visiting);
                   intrinsic_call != nullptr) {
            CHECK(intrinsic_call->next != nullptr);
            frontier.push(intrinsic_call->next);
        } else if (const auto * struct_init
                   = dynamic_cast<const control_flow::struct_init *>(visiting);
                   struct_init != nullptr) {
            CHECK(struct_init->next != nullptr);
            frontier.push(struct_init->next);
        } else if (const auto * func_call
                   = dynamic_cast<const control_flow::function_call *>(visiting);
                   func_call != nullptr) {
            CHECK(func_call->next != nullptr);
            frontier.push(func_call->next);
        } else if (const auto * member_access
                   = dynamic_cast<const control_flow::member_access *>(visiting);
                   member_access != nullptr) {
            CHECK(member_access->next != nullptr);
            frontier.push(member_access->next);
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

    auto lowering = ast_to_cfg{ty_context};

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

    auto lowering = ast_to_cfg{ty_context};

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

TEST_CASE("ast_to_cfg can lower an let of struct followed by a return") {
    std::string buffer = "foo {}\nlol() { let x = foo {}; return; }";
    std::cout << "Testing " << buffer << std::endl;

    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);
    auto mod = parser->parse();

    auto lowering = ast_to_cfg{ty_context};

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

TEST_CASE("ast_to_cfg can lower a struct init followed by an intrinsic followed by a return of the "
          "struct") {
    std::string buffer = "bar {z : bool}\n foo{ y : int32, sub : bar }\nlol() -> foo { let x = "
                         "foo{ y = 0, sub = bar { z = false } }; syscall(1, x); return x;}";
    std::cout << "Testing " << buffer << std::endl;

    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);
    auto mod = parser->parse();

    auto lowering = ast_to_cfg{ty_context};

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

TEST_CASE("ast_to_cfg can lower an intrinsic followed by a return") {
    std::string buffer = "lol() { syscall(); return; }";
    std::cout << "Testing " << buffer << std::endl;

    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);
    auto mod = parser->parse();

    auto lowering = ast_to_cfg{ty_context};

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

TEST_CASE("ast_to_cfg can lower a function call followed by an intrinsic call") {
    std::string buffer = "bar { lol : bool } foo() -> bar { return bar {lol = true}; } lol() { let "
                         "x = foo(); let y = syscall(x.lol); }";
    std::cout << "Testing " << buffer << std::endl;

    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);
    auto mod = parser->parse();

    auto lowering = ast_to_cfg{ty_context};

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

        CHECK_FALSE(func_start->exported);
        CHECK(func_start->next != nullptr);
    });
    CHECK(root_count == 2);

    // The last node generated should be a function end.
    CHECK(cfg->previous_node() != nullptr);
    auto * func_end = dynamic_cast<control_flow::function_end *>(cfg->previous_node());
    CHECK(func_end != nullptr);

    scan_graph(start, func_end);
}

TEST_CASE("ast_to_cfg can lower 2 let-intrinsic calls") {
    std::string buffer = "main () { let x = syscall(); let y = syscall(x); if y == 1 then syscall(); }";
    std::cout << "Testing " << buffer << std::endl;

    ast::type_context ty_context;
    auto parser = parser::from_buffer(buffer, ty_context);
    auto mod = parser->parse();

    auto lowering = ast_to_cfg{ty_context};

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