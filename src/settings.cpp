#include "settings.hpp"

#include "lyra/lyra.hpp"

#include <iostream>

std::shared_ptr<Settings> read_settings(int arg_count, const char * const * args) {

    auto settings = std::make_shared<Settings>();

    auto print_help = false;

    auto cli = lyra::help(print_help) | lyra::opt(settings->print_version)["--version"]["-V"]
             | lyra::opt(settings->simulate)["--sim"]
             | lyra::opt(settings->debug)["--debug"]
             | lyra::arg(settings->file_to_read, "file to read");

    if (auto result = cli.parse({arg_count, args}); not result) {
        std::cerr << "Error in command line : " << result.errorMessage() << std::endl;
        exit(1);
    }

    if (print_help) {
        std::cout << cli << std::endl;
        exit(0);
    }

    return settings;
}
