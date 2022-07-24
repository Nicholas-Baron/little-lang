add_library(test_runner STATIC main_tests.cpp)
target_link_libraries(test_runner CONAN_PKG::catch2)

foreach(test settings_test string_utils_test new_parser_test new_lexer_test)
    string(CONCAT filename ${test} ".cpp")
    add_executable(${test} ${filename})

    target_link_libraries(${test} littlec_lib test_runner CONAN_PKG::lyra)
    add_test(NAME "${test}" COMMAND $<TARGET_FILE:${test}>)
endforeach()
