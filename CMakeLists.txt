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

# Flex and Bison are needed
find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)

# Format the LLVM flags
execute_process(COMMAND llvm-config --cxxflags --ldflags --libs core native OUTPUT_VARIABLE LLVM_CONFIG)
string(REGEX REPLACE "[\n\t ]+" " " LLVM_CONFIG "${LLVM_CONFIG}")
separate_arguments(LLVM_CONFIG)

# Status messages
message(STATUS "LLVM options: ${LLVM_CONFIG}")
message(STATUS "Source dir: ${CMAKE_SOURCE_DIR}")

# the directory for temporary files
set(temp_dir ${CMAKE_SOURCE_DIR}/temp)
# the header from Bison
set(parser_header ${temp_dir}/parser.hpp)
# the bison spec file
set(bison_input ${CMAKE_SOURCE_DIR}/src/parser.y)
# the main output of bison
set(bison_output ${temp_dir}/parser.cpp)

add_custom_command(
	OUTPUT ${bison_output} ${parser_header}
	COMMAND ${BISON_EXECUTABLE} ${bison_input} --defines=${parser_header} --report=state --graph --debug -Wall --output=${bison_output}
	COMMENT "Generating parser..."
	USES_TERMINAL
	)

set(flex_input ${CMAKE_SOURCE_DIR}/src/tokens.l ${parser_header})
set(flex_output ${temp_dir}/tokens.cpp)

add_custom_command(
	OUTPUT ${flex_output}
	COMMAND ${FLEX_EXECUTABLE} --header-file=${temp_dir}/tokens.hpp -o ${flex_output} ${flex_input}
	COMMENT "Generating tokenizer..."
	WORKING_DIRECTORY ${temp_dir}
	DEPENDS ${parser_header}
	USES_TERMINAL
	)

set(proj_files
	${CMAKE_SOURCE_DIR}/src/main.cpp
	${CMAKE_SOURCE_DIR}/src/nodes.cpp
	${CMAKE_SOURCE_DIR}/src/location.cpp
	${CMAKE_SOURCE_DIR}/src/context_module.cpp
	${CMAKE_SOURCE_DIR}/src/settings.cpp
	)

add_executable(littlec ${bison_output} ${flex_output} ${proj_files})

target_compile_options(littlec PRIVATE -Wall -Wextra -Og ${LLVM_CONFIG} -Wno-unused-command-line-argument -Wno-unused-parameter)
target_link_libraries(littlec ${LLVM_CONFIG})
target_include_directories(littlec PRIVATE ${CMAKE_SOURCE_DIR}/src ${temp_dir} ${CMAKE_SOURCE_DIR}/external)

add_custom_target(
    copy-compile-commands ALL
    ${CMAKE_COMMAND} -E copy_if_different
        ${CMAKE_BINARY_DIR}/compile_commands.json
        ${CMAKE_CURRENT_LIST_DIR}
    )
