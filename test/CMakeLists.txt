add_executable(littlec_test settings_test.cpp main_tests.cpp)
target_link_libraries(littlec_test littlec_lib CONAN_PKG::catch2 CONAN_PKG::lyra)
add_test(NAME littlec_test COMMAND $<TARGET_FILE:littlec_test>)
