find_package(Catch2 REQUIRED)
find_package(lyra REQUIRED)

add_library(test_runner STATIC main_tests.cpp)
target_link_libraries(test_runner Catch2::Catch2)

foreach(
    test
    ast_to_cfg_test
    global_map_test
    new_lexer_test
    new_parser_test
    scoped_map_test
    serializer_test
    settings_test
    string_utils_test
)
    string(CONCAT filename ${test} ".cpp")
    add_executable(${test} ${filename})

    target_link_libraries(${test} littlec_lib test_runner bfg::lyra)
    add_test(NAME "${test}" COMMAND $<TARGET_FILE:${test}>)
endforeach()

add_executable(fuzzer fuzzer.cpp)
target_link_libraries(fuzzer littlec_ast)
target_compile_options(fuzzer PRIVATE -fsanitize=fuzzer,address,undefined)
target_link_options(fuzzer PRIVATE -fsanitize=fuzzer,address,undefined)
