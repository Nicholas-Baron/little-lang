foreach(test settings_test string_utils_test new_parser_test)
	string(CONCAT filename ${test} ".cpp")
	add_executable(${test} ${filename})

	target_link_libraries(${test} littlec_lib CONAN_PKG::catch2 CONAN_PKG::lyra)
	add_test(NAME "${test}" COMMAND $<TARGET_FILE:${test}>)
endforeach()
