#include "global_map.hpp"

#include <catch2/catch.hpp>

using test_global_map = global_map<std::string, const char *>;

TEST_CASE("global_map will fail lookup if the module does not exist") {
    test_global_map test_map;

    CHECK(test_map.lookup("nonexistent", "nonexistent") == nullptr);
}

TEST_CASE("global_map will fail lookup if the key does not exist") {
    test_global_map test_map;

    test_map.add("module", "key", "test");

    CHECK(test_map.lookup("module", "nonexistent") == nullptr);
}

TEST_CASE("global_map will succeed lookup if both the key and module exist") {
    test_global_map test_map;
    const auto * value = "test";

    test_map.add("module", "key", value);

    CHECK(test_map.lookup("module", "key") == value);
}
