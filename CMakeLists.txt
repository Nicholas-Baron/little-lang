cmake_minimum_required(VERSION 3.1)

set(CMAKE_CXX_COMPILER clang++)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS YES)

project(
	Little_Compiler
	LANGUAGES CXX
	)

find_program(CONAN conan)
if(NOT CONAN)
	message(FATAL_ERROR "Could not find conan package manager.")
endif()

include(${CMAKE_SOURCE_DIR}/cmake/conan.cmake)

conan_check(VERSION 1.14.4 REQUIRED)
conan_cmake_run(CONANFILE conanfile.txt
        BASIC_SETUP CMAKE_TARGETS
        BUILD missing
		)

enable_testing()

# Flex and Bison are needed
find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)

# Format the LLVM flags
execute_process(
	COMMAND llvm-config --cxxflags --ldflags --libs core native
	OUTPUT_VARIABLE LLVM_CONFIG
)
string(REGEX REPLACE "[\n\t ]+" " " LLVM_CONFIG "${LLVM_CONFIG}")
separate_arguments(LLVM_CONFIG)

# Status messages
message(STATUS "LLVM options: ${LLVM_CONFIG}")

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
add_compile_options(-Wundef)
add_compile_options(-Wunused)
add_compile_options(-Wwrite-strings)
add_compile_options(-ftime-trace)

add_subdirectory(src)
add_subdirectory(test)

add_custom_target(
    copy-compile-commands ALL
    ${CMAKE_COMMAND} -E copy_if_different
        ${CMAKE_BINARY_DIR}/compile_commands.json
        ${CMAKE_CURRENT_LIST_DIR}
    )
