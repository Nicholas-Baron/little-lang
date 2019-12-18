#include "context_module.hpp"
#include "nodes.hpp"
#include "settings.hpp"

#include "parser.hpp"
#include "tokens.hpp"

#include <cassert>
#include <fstream>
#include <iostream>
#include <sstream>

extern std::unique_ptr<Top_Level_Seq> module;

std::string read_file(const std::string & name) {

	std::stringstream content;

	{
		std::ifstream file{name};
		std::string   line;
		while (getline(file, line)) { content << line << '\n'; }
	}

	return content.str();
}

bool parse_file(const std::string & content) {

	auto buffer = yy_scan_string(content.c_str());

	yy_switch_to_buffer(buffer);

	const auto parse_status = yyparse();

	yy_delete_buffer(buffer);

	if (parse_status != 0) {
		std::cerr << "Parsing error: ";
		if (parse_status == 1) {
			std::cerr << "Syntax error found!";
		} else if (parse_status == 2) {
			std::cerr << "Exhausted memory!";
		} else {
			std::cerr << "Unknown error";
		}

		std::cerr << std::endl;
	}

	/* According to bison, a yyparse() result of 0 is successful.
	 * (ie: read all input) */
	return parse_status == 0;
}

int main(const int arg_count, const char * const * const args) {

	const auto command_line = read_settings(arg_count, args);

	const auto & filename = command_line->file_to_read;

	const auto content = read_file(filename);

	if (content.empty()) {
		std::cerr << "Input file is empty -> nothing to parse" << std::endl;
		return -1;
	}

	const auto success = parse_file(content);

	if (not success) {
		std::cerr << "Failed to parse " << filename << std::endl;
		return -1;
	}

	assert(module != nullptr);

	context_module context{filename};

	module->codegen(context);

	context.dump();
}
