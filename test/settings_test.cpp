#include "settings.hpp"
#include <catch2/catch.hpp>

#include <array>

TEST_CASE("Command line settings parse", "[Settings]") {
    SECTION("file names are parsed correctly") {
        std::array args{"littlec", "input.txt"};
        auto settings = read_settings(args.size(), args.data());
        CHECK(settings != nullptr);
        CHECK(settings->print_version == false);
        CHECK(settings->file_to_read == "input.txt");
    }
}
