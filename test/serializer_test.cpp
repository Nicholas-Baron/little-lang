#include "ast/nodes.hpp"
#include "ast/parser.hpp"
#include "ast/serializer.hpp"

#include <iostream>
#include <sstream>

#include <catch2/catch.hpp>

using namespace nlohmann::json_literals;

TEST_CASE("serializer handles hello world") {
    std::string buffer = R"(
	main() {
		let output = "hello world";
		print(output);
	}
	)";

    ast::type_context type_context;
    auto parser = parser::from_buffer(buffer, type_context);
    REQUIRE(parser != nullptr);

    auto mod = parser->parse();
    REQUIRE(mod != nullptr);

    auto serialized = ast::serializer::to_json("", *mod);

    CHECK(serialized.is_object());
    CHECK(serialized.size() == 2);
    CHECK(serialized == R"({
  "contents": [
    {
      "body": [
        {
		  "decl_type" : "let",
          "value": {
            "value": "\"hello world\"",
            "value_type": "string"
          },
          "variable": {
            "name": "output",
            "type": null
          }
        },
        {
          "args": [
            {
              "value": "output",
              "value_type": "identifier"
            }
          ],
          "name": "print"
        }
      ],
      "name": "main",
      "paramaters": [],
      "return type": "unit"
    }
  ],
  "filename": ""
})"_json);
}
