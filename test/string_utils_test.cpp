#include "utils/string_utils.hpp"

#define CATCH_CONFIG_MAIN
#include <catch2/catch.hpp>

TEST_CASE("unquote preserves unpaired quotes") { CHECK(unquote("\'") == "\'"); }

TEST_CASE("unquote preserves internal quotes") { CHECK(unquote("The \'dog\'") == "The \'dog\'"); }

TEST_CASE("unquote removes external double quotes") {
    CHECK(unquote("\"file.txt\"") == "file.txt");
}
