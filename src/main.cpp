#include "context_module.hpp"

#include <iostream>

extern int yyparse();

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
	context_module context{filename};
}
