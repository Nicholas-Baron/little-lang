add_executable(littlec_test
	emit_asm_test.cpp
	main_tests.cpp
	settings_test.cpp
	parser_test.cpp
	)
target_link_libraries(littlec_test littlec_lib CONAN_PKG::catch2 CONAN_PKG::lyra)
add_test(NAME littlec_test COMMAND $<TARGET_FILE:littlec_test>)
