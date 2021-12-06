#include "new_parser.hpp"
#include <catch2/catch.hpp>

TEST_CASE("the input cannot be empty") {
    std::string buffer;
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);
    CHECK(parser->parse() == nullptr);
    CHECK(parser->error_message() == "Found empty file");
}

TEST_CASE("a function can be parsed") {
    std::string buffer = "main() -> int = 0";
    auto parser = parser::from_buffer(buffer);

    CHECK(parser != nullptr);
    CHECK(parser->parse() != nullptr);
    CHECK(parser->error_message().empty());
}
