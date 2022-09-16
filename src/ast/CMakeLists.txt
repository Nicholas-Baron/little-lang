add_library(littlec_type STATIC type.cpp)

target_compile_options(
    littlec_type PRIVATE -Og -fdata-sections -ffunction-sections
)
target_link_libraries(littlec_type littlec_utils)

add_library(
    littlec_ast STATIC lexer.cpp location.cpp nodes.cpp parser.cpp
                       serializer.cpp token_to_string.cpp
)

target_compile_options(
    littlec_ast PRIVATE -Og -fdata-sections -ffunction-sections
)
target_link_libraries(
    littlec_ast littlec_utils littlec_type CONAN_PKG::nlohmann_json
)
