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
	main.cpp
	context_module.cpp
	emit_asm.cpp
	jit.cpp
	location.cpp
	nodes.cpp
	settings.cpp
	)

add_executable(littlec ${bison_output} ${flex_output} ${proj_files})

target_compile_options(littlec
	PRIVATE -Og ${LLVM_CONFIG} -Wno-unused-command-line-argument -Wno-unused-parameter
	)
target_link_libraries(littlec ${LLVM_CONFIG} CONAN_PKG::lyra)
target_include_directories(littlec
	PRIVATE ${CMAKE_SOURCE_DIR}/src ${temp_dir} CONAN_PKG::lyra
	)
