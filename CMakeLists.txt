cmake_minimum_required(VERSION 3.1)

set(CMAKE_CXX_COMPILER clang++)
set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

project(Little_Compiler)

message(STATUS "Source dir is ${CMAKE_SOURCE_DIR}")

find_package(FLEX REQUIRED)
find_package(BISON REQUIRED)

execute_process(COMMAND llvm-config --cxxflags --ldflags --libs core native OUTPUT_VARIABLE LLVM_CONFIG)
string(REGEX REPLACE "[\n\t ]+" " " LLVM_CONFIG ${LLVM_CONFIG})
separate_arguments(LLVM_CONFIG)
message(STATUS "LLVM options: ${LLVM_CONFIG}")

set(bison_input ${CMAKE_SOURCE_DIR}/src/parser.y)
set(bison_output ${CMAKE_SOURCE_DIR}/src/parser.cpp)
add_custom_command(
	OUTPUT ${bison_output}
	COMMAND ${BISON_EXECUTABLE} ${bison_input} -d --report=state -t --graph -Wall --output=${bison_output}
	COMMENT "Generating parser..."
	USES_TERMINAL
	)

set(flex_input ${CMAKE_SOURCE_DIR}/src/tokens.l ${CMAKE_SOURCE_DIR}/src/parser.hpp)
set(flex_output ${CMAKE_SOURCE_DIR}/src/tokens.cpp)
add_custom_command(
	OUTPUT ${flex_output}
	COMMAND ${FLEX_EXECUTABLE} --header-file=${CMAKE_SOURCE_DIR}/src/tokens.hpp -o ${flex_output} ${flex_input}
	COMMENT "Generating tokenizer..."
	WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/src
	USES_TERMINAL
	)

add_executable(littlec ${CMAKE_SOURCE_DIR}/src/main.cpp ${bison_output} ${flex_output})
target_compile_options(littlec PRIVATE -Wall ${LLVM_CONFIG})
target_link_libraries(littlec ${LLVM_CONFIG})
target_include_directories(littlec PRIVATE ${CMAKE_SOURCE_DIR}/src)

