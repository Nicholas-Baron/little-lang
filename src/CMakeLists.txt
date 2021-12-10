# Bison is needed only for checking the formal grammar
find_package(BISON)

if(BISON_FOUND)
	# the bison spec file
	set(bison_input ${CMAKE_SOURCE_DIR}/src/parser.y)
	add_custom_target(bison
		COMMAND ${BISON_EXECUTABLE} ${bison_input} -Wall --verbose --report-file=parser.report
		WORKING_DIRECTORY ${CMAKE_BINARY_DIR}
		VERBATIM USES_TERMINAL
		)
endif()

set(proj_files
	emit_asm.cpp
	jit.cpp
	location.cpp
	new_parser.cpp
	program.cpp
	ast/nodes.cpp
	visitor/codegen.cpp
	visitor/type_checker.cpp
	visitor/token_to_string.cpp
	visitor/printer.cpp
	utils/string_utils.cpp
	settings.cpp
	)

# Add reserved identifier warning here, as catch's internals seem to set it off a lot
if(CMAKE_CXX_COMPILER_VERSION VERSION_GREATER_EQUAL "13")
	add_compile_options(-Wreserved-identifier)
endif()

# the actual executable
add_executable(littlec main.cpp ${proj_files})
target_compile_options(littlec
	PRIVATE -O2 ${LLVM_CONFIG} -Wno-unused-command-line-argument -Wno-unused-parameter
	)
target_link_libraries(littlec ${LLVM_CONFIG} CONAN_PKG::lyra)
target_include_directories(littlec
	PRIVATE ${CMAKE_SOURCE_DIR}/src CONAN_PKG::lyra
	)

# the library for testing
add_library(littlec_lib ${proj_files})
target_compile_options(littlec_lib
	PUBLIC -Og ${LLVM_CONFIG} -Wno-unused-command-line-argument -Wno-unused-parameter
	)
target_link_libraries(littlec_lib ${LLVM_CONFIG} CONAN_PKG::lyra)
target_include_directories(littlec_lib
	PUBLIC ${CMAKE_SOURCE_DIR}/src
	PRIVATE CONAN_PKG::lyra
	)
