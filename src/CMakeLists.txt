# Bison is needed only for checking the formal grammar
find_package(BISON)

if(BISON_FOUND)
    # the bison spec file
    set(bison_input ${CMAKE_SOURCE_DIR}/src/parser.y)
    add_custom_target(
        bison
        COMMAND ${BISON_EXECUTABLE} ${bison_input} -Wall --verbose
                --report-file=parser.report
        WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
        VERBATIM USES_TERMINAL
    )
endif()

configure_file(
    ${CMAKE_SOURCE_DIR}/src/version.hpp.in ${CMAKE_BINARY_DIR}/version.hpp
    @ONLY
)

set(proj_files
    ast_to_cfg.cpp
    emit_asm.cpp
    jit.cpp
    llvm_type_lowering.cpp
    program.cpp
    settings.cpp
    control_flow/graph.cpp
    control_flow/serializer.cpp
    visitor/codegen.cpp
    visitor/type_checker.cpp
)

# Add reserved identifier warning here, as catch's internals seem to set it off
# a lot
if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL "13")
    add_compile_options(-Wreserved-identifier)
endif()

# Get utils and common first, as other libraries may need them
add_subdirectory(common)
add_subdirectory(utils)
add_subdirectory(ast)

# the library for testing
add_library(littlec_lib ${proj_files})
target_compile_options(
    littlec_lib
    PRIVATE -Og -fdata-sections -ffunction-sections
    PUBLIC -Wno-unused-command-line-argument
)
target_link_libraries(littlec_lib littlec_ast littlec_utils)

llvm_map_components_to_libnames(
    LLVM_LIBS
    all
    core
    support
    native
    executionengine
    asmprinter
    codegen
    passes
)

target_link_libraries(littlec_lib ${LLVM_LIBS} CONAN_PKG::lyra)
target_link_options(littlec_lib PUBLIC -Wl,--gc-sections)
target_include_directories(
    littlec_lib
    PUBLIC ${CMAKE_SOURCE_DIR}/src
    PRIVATE CONAN_PKG::lyra ${CMAKE_BINARY_DIR} ${LLVM_INCLUDE_DIRS}
)
separate_arguments(LLVM_DEF_LIST NATIVE_COMMAND ${LLVM_DEFINITIONS})
target_compile_definitions(littlec_lib PRIVATE ${LLVM_DEF_LIST})
target_precompile_headers(
    littlec_lib
    PRIVATE
    ast/expr_nodes.hpp
    ast/lexer.hpp
    ast/location.hpp
    ast/parser.hpp
    ast/stmt_nodes.hpp
    emit_asm.hpp
    program.hpp
    utils/global_map.hpp
    visitor/codegen.hpp
)

# the actual executable
add_executable(littlec main.cpp)
target_compile_options(
    littlec PRIVATE -O2 -Wno-unused-command-line-argument -Wno-unused-parameter
)
target_link_libraries(littlec CONAN_PKG::lyra littlec_lib)
target_include_directories(
    littlec PRIVATE ${CMAKE_SOURCE_DIR}/src CONAN_PKG::lyra ${CMAKE_BINARY_DIR}
)
