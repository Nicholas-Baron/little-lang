add_library(littlec_type STATIC type.cpp type_context.cpp)

target_compile_options(
    littlec_type PRIVATE -Og -fdata-sections -ffunction-sections
)
target_link_libraries(littlec_type littlec_utils)

add_library(
    littlec_ast STATIC lexer.cpp location.cpp nodes.cpp parser.cpp
                       serializer.cpp
)

target_compile_options(
    littlec_ast PRIVATE -Og -fdata-sections -ffunction-sections
)
target_link_libraries(
    littlec_ast PUBLIC littlec_utils littlec_type littlec_common
                       CONAN_PKG::nlohmann_json
)
