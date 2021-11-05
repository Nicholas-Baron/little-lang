#include "settings.hpp"
#include <catch2/catch.hpp>

#include <array>

TEST_CASE("file names are parsed correctly") {
    std::array args{"littlec", "input.txt"};
    auto settings = read_settings(args.size(), args.data());
    CHECK(settings != nullptr);
    CHECK(settings->print_version == false);
    CHECK(settings->file_to_read == "input.txt");
    CHECK(settings->simulate == false);
}

TEST_CASE("Simulate flag is parsed correctly") {
    std::array args{"littlec", "--sim", "input.txt"};
    auto settings = read_settings(args.size(), args.data());
    CHECK(settings != nullptr);
    CHECK(settings->print_version == false);
    CHECK(settings->file_to_read == "input.txt");
    CHECK(settings->simulate == true);
}
