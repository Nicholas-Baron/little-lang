# Bison is needed only for checking the formal grammar
find_package(BISON)

if(BISON_FOUND)
  # the bison spec file
  set(bison_input ${CMAKE_SOURCE_DIR}/src/parser.y)
  add_custom_target(
    bison
    COMMAND ${BISON_EXECUTABLE} ${bison_input} -Wall --verbose --report-file=parser.report
    WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
    VERBATIM USES_TERMINAL
  )
endif()

configure_file(${CMAKE_SOURCE_DIR}/src/version.hpp.in ${CMAKE_BINARY_DIR}/version.hpp @ONLY)

set(
  proj_files
  ast/nodes.cpp
  ast/type.cpp
  emit_asm.cpp
  jit.cpp
  location.cpp
  new_lexer.cpp
  new_parser.cpp
  program.cpp
  settings.cpp
  type_context.cpp
  utils/execute.cpp
  utils/string_utils.cpp
  visitor/codegen.cpp
  visitor/printer.cpp
  visitor/token_to_string.cpp
  visitor/type_checker.cpp
)

# Add reserved identifier warning here, as catch's internals seem to set it off a lot
if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL "13")
  add_compile_options(-Wreserved-identifier)
endif()

# the library for testing
add_library(littlec_lib ${proj_files})
target_compile_options(
  littlec_lib
  PRIVATE -Og
    -fdata-sections -ffunction-sections
  PUBLIC ${LLVM_CONFIG}
    -Wno-unused-command-line-argument
)
target_link_libraries(littlec_lib ${LLVM_CONFIG} CONAN_PKG::lyra)
target_link_options(littlec_lib PUBLIC -Wl,--gc-sections)
target_include_directories(
  littlec_lib
  PUBLIC ${CMAKE_SOURCE_DIR}/src
  PRIVATE CONAN_PKG::lyra ${CMAKE_BINARY_DIR}
)
target_precompile_headers(littlec_lib
	PUBLIC ast/nodes.hpp program.hpp
)


# the actual executable
add_executable(littlec main.cpp)
target_compile_options(
  littlec
  PRIVATE -O2 ${LLVM_CONFIG}
    -Wno-unused-command-line-argument -Wno-unused-parameter
)
target_link_libraries(littlec ${LLVM_CONFIG} CONAN_PKG::lyra littlec_lib)
target_include_directories(
  littlec
  PRIVATE ${CMAKE_SOURCE_DIR}/src CONAN_PKG::lyra ${CMAKE_BINARY_DIR}
)

