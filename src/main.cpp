#include "context_module.hpp"
#include "nodes.hpp"
#include "settings.hpp"

#include "tokens.hpp"

#include <cassert>
#include <fstream>
#include <iostream>
#include <sstream>

extern std::unique_ptr<Top_Level_Seq> module;
extern int							  yyparse();

std::string read_file(const std::string & name) {

	std::stringstream content;

	{
		std::ifstream file{name};
		std::string   line;
		while (getline(file, line)) { content << line << '\n'; }
	}

	return content.str();
}

int main(const int arg_count, const char * const * const args) {

	const auto filename = [&]() -> std::string {
		const char * name = nullptr;
		for (int i = 1; i < arg_count; i++) {
			if (args[i][0] != '-') { name = args[i]; }
		}

		if (name != nullptr) { return {name}; }

		std::cout << "Enter a file to compile: " << std::flush;
		std::string data;
		std::cin >> data;
		return data;
	}();

	auto content = read_file(filename);

	if (content.empty()) {
		std::cerr << "File is empty" << std::endl;
		return -1;
	}

	// std::cout << "Opened file. Initializing parser..." << std::endl;

	auto buffer = yy_scan_string(content.data());

	yy_switch_to_buffer(buffer);

	// std::cout << "Parsing " << filename << "..." << std::endl;

	const auto parse_status = yyparse();

	if (parse_status != 0) {
		std::cerr << "Parsing error: ";
		if (parse_status == 1) {
			std::cerr << "Syntax error found!" << std::endl;
		} else if (parse_status == 2) {
			std::cerr << "Exhausted memory!" << std::endl;
		} else {
			std::cerr << "Unknown error" << std::endl;
		}
	}

	yy_delete_buffer(buffer);

	// std::cout << "Done parsing. Initializing LLVM..." << std::endl;

	context_module context{filename};

	assert(module != nullptr);

	// std::cout << "Generating code..." << std::endl;
	module->codegen(context);

	context.dump();
}
