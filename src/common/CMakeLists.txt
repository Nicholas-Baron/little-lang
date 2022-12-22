add_library(littlec_common STATIC token_to_string.cpp)

target_compile_options(littlec_common PRIVATE -Og)
target_include_directories(littlec_common PUBLIC .)
