add_library(littlec_utils STATIC execute.cpp string_utils.cpp)

target_include_directories(littlec_utils PUBLIC .)

target_compile_options(
    littlec_utils PRIVATE -Og -fdata-sections -ffunction-sections
)
