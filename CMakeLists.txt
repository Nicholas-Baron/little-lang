cmake_minimum_required(VERSION 3.1)

set(CMAKE_CXX_COMPILER clang++)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

project(
	Little_Compiler
	LANGUAGES CXX
	)

# Flex and Bison are needed
find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)

# Format the LLVM flags
execute_process(COMMAND llvm-config-9 --cxxflags --ldflags --libs core native OUTPUT_VARIABLE LLVM_CONFIG)
string(REGEX REPLACE "[\n\t ]+" " " LLVM_CONFIG "${LLVM_CONFIG}")

separate_arguments(LLVM_CONFIG)

# Status messages
message(STATUS "LLVM options: ${LLVM_CONFIG}")
message(STATUS "Source dir: ${CMAKE_SOURCE_DIR}")

set(temp_dir ${CMAKE_SOURCE_DIR}/temp)

set(parser_header ${temp_dir}/parser.hpp)

set(bison_input ${CMAKE_SOURCE_DIR}/src/parser.y)
set(bison_output ${temp_dir}/parser.cpp)

add_custom_command(
	OUTPUT ${bison_output} ${parser_header}
	COMMAND ${BISON_EXECUTABLE} ${bison_input} --defines=${parser_header} --report=state -t --graph -Wall --output=${bison_output}
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

set(proj_files ${CMAKE_SOURCE_DIR}/src/main.cpp ${CMAKE_SOURCE_DIR}/src/nodes.cpp ${CMAKE_SOURCE_DIR}/src/location.cpp ${CMAKE_SOURCE_DIR}/src/context_module.cpp ${CMAKE_SOURCE_DIR}/src/settings.cpp)
set(input_files ${bison_output} ${flex_output} ${proj_files})

add_executable(littlec ${input_files})
target_compile_options(littlec PRIVATE -Wall -Wextra -g ${LLVM_CONFIG} -Wno-unused-command-line-argument -Wno-unused-parameter)
target_link_libraries(littlec ${LLVM_CONFIG})
target_include_directories(littlec PRIVATE ${CMAKE_SOURCE_DIR}/src ${temp_dir} ${CMAKE_SOURCE_DIR}/external)

