add_library(
    littlec_ast STATIC
    lexer.cpp
    location.cpp
    nodes.cpp
    parser.cpp
    serializer.cpp
    token_to_string.cpp
    type.cpp
)

target_compile_options(
    littlec_ast PRIVATE -Og -fdata-sections -ffunction-sections
)
target_link_libraries(littlec_ast littlec_utils CONAN_PKG::nlohmann_json)
