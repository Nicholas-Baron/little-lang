#include "settings.hpp"

#include "clara.hpp"

#include <iostream>

std::shared_ptr<Settings> read_settings(int					 arg_count,
										const char * const * args) {

	Settings settings{};

	using namespace clara;

	auto cli = Opt(settings.print_version, "print version")["-V"]["--version"](
				   "Print the version of the compiler")
			   | Arg(settings.file_to_read,
					 "input file")("The source code to compile")
			   | Help(settings.print_help);

	auto result = cli.parse(Args(arg_count, args));

	if (not result) {
		std::cerr << "Error in command line : " << result.errorMessage()
				  << std::endl;
		exit(1);
	}

	return std::make_shared<Settings>(settings);
}
