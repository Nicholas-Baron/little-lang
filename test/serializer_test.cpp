#include "ast/parser.hpp"
#include "ast/top_lvl_nodes.hpp"
#include "ast/serializer.hpp"

#include <iostream>
#include <sstream>

#include <catch2/catch.hpp>

TEST_CASE("serializer handles hello world") {
    std::string buffer = R"(
	main() {
		let output = "hello world";
		print(output);
	}
	)";

    auto parser = parser::from_buffer(buffer);
    REQUIRE(parser != nullptr);

    auto mod = parser->parse();
    REQUIRE(mod != nullptr);

    auto serialized = ast::serializer::to_json("", *mod);

    CHECK(serialized.is_object());
    CHECK(serialized.size() == 2);
	std::cout << serialized;
}
