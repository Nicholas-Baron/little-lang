cmake_minimum_required(VERSION 3.7)

if("${CMAKE_CXX_COMPILER}" STREQUAL "")
    set(CMAKE_CXX_COMPILER clang++)
else()
    message(STATUS "Using ${CMAKE_CXX_COMPILER}")
endif()

set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS YES)
cmake_policy(SET CMP0048 NEW)
cmake_policy(SET CMP0115 NEW)

project(Little_Compiler VERSION 0.0.1)

find_program(CONAN conan)
if(NOT CONAN)
    message(FATAL_ERROR "Could not find conan package manager.")
endif()

include(${CMAKE_SOURCE_DIR}/cmake/conan.cmake)

conan_check(VERSION 1.14.4 REQUIRED)
conan_cmake_run(CONANFILE conanfile.txt BASIC_SETUP CMAKE_TARGETS BUILD missing)

find_program(CCACHE ccache)
if(CCACHE)
    set_property(GLOBAL PROPERTY RULE_LAUNCH_COMPILE ccache)
    message("Using ccache")
endif()

enable_testing()

find_package(LLVM REQUIRED CONFIG)

# Status messages
message(STATUS "Using LLVM ${LLVM_PACKAGE_VERSION}")

# the directory for temporary files
set(temp_dir ${CMAKE_BINARY_DIR}/temp)

# project wide compile options
add_compile_options(-Wall)
add_compile_options(-Wextra)
add_compile_options(-Werror)
add_compile_options(-Wpedantic)
add_compile_options(-Wcast-qual)
add_compile_options(-Wdeprecated-copy)
add_compile_options(-Wformat=2)
add_compile_options(-Wimplicit-fallthrough)
add_compile_options(-Wmisleading-indentation)
add_compile_options(-Wmissing-declarations)
add_compile_options(-Wnon-virtual-dtor)
add_compile_options(-Wnull-dereference)
add_compile_options(-Wundef)
add_compile_options(-Wunused)
add_compile_options(-Wwrite-strings)
add_compile_options(-ftime-trace)

if(${CMAKE_BUILD_TYPE} STREQUAL "Debug")
	option(ASAN "Build with ASAN (LLVM will spam a lot)" OFF)
	if(ASAN)
		add_compile_options(-fsanitize=undefined,address)
		add_link_options(-lasan -lubsan)
	else()
		add_compile_options(-fsanitize=undefined)
		add_link_options(-lubsan)
	endif()
endif()

add_subdirectory(src)
add_subdirectory(test)

add_custom_target(
    copy-compile-commands ALL
    ${CMAKE_COMMAND} -E copy_if_different
    ${CMAKE_BINARY_DIR}/compile_commands.json ${CMAKE_CURRENT_LIST_DIR}
)
