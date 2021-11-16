#include "emit_asm.hpp"
#include <catch2/catch.hpp>

TEST_CASE("output file names are correctly generated") {
    SECTION("The output will always end up in the current directory") {
        CHECK(make_output_name("fib.txt") == "fib.o");
        CHECK(make_output_name("/fib.txt") == "fib.o");
        CHECK(make_output_name("my_cool_project/fib.txt") == "fib.o");
        CHECK(make_output_name("./fib.txt") == "fib.o");
        CHECK(make_output_name("../fib.txt") == "fib.o");
    }

    SECTION("The input can have dots in the name") {
        CHECK(make_output_name("fib.txt.txt") == "fib.txt.o");
        CHECK(make_output_name("fib/txt.txt") == "txt.o");
    }
}
