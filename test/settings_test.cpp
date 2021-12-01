#include "settings.hpp"
#include <catch2/catch.hpp>

#include <array>

// TODO: Test debug flags

TEST_CASE("file names are parsed correctly") {
    std::array args{"littlec", "input.txt"};
    auto settings = read_settings(args.size(), args.data());
    CHECK(settings != nullptr);
    CHECK(settings->file_to_read == "input.txt");
    CHECK_FALSE(settings->flag_is_set(cmd_flag::simulate));
}

TEST_CASE("Simulate flag is parsed correctly") {
    std::array args{"littlec", "--sim", "input.txt"};
    auto settings = read_settings(args.size(), args.data());
    CHECK(settings != nullptr);
    CHECK(settings->file_to_read == "input.txt");
    CHECK(settings->flag_is_set(cmd_flag::simulate));
}
