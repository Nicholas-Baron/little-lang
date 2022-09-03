#include "utils/scoped_map.hpp"

#include <catch2/catch.hpp>

using test_scoped_map = scoped_map<std::string, int>;

TEST_CASE("the inital current scope of a scoped_map is the root scope") {
    test_scoped_map test_map;

    auto entry = test_map.add_to_root("test", 5);

    test_map.add_to_current_scope("test", 10);

    CHECK(entry.first->second == 5);
}

TEST_CASE("inserted values cannot be overridden") {
    test_scoped_map test_map;

    auto entry = test_map.add_to_root("test", 5);

    test_map.add_to_current_scope("test", 10);

    CHECK(entry.first->second == 5);
}

TEST_CASE("scopes are walked in reverse") {
    test_scoped_map test_map;

    test_map.add_to_root("test", 2);

    {
        auto & scope = test_map.add_scope();
        scope.emplace("test", 1);
    }

    test_map.add_scope().emplace("test", 0);

    std::vector<int> values;
    for (auto & scope : test_map) { values.push_back(scope.at("test")); }
    CHECK(std::is_sorted(values.begin(), values.end()));
}
