add_library(utils STATIC execute.cpp string_utils.cpp)

target_include_directories(utils PUBLIC .)

target_compile_options(utils PRIVATE -Og -fdata-sections -ffunction-sections)
