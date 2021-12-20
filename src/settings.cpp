#include "settings.hpp"

#include "lyra/lyra.hpp"

#include <iostream>
#include <version.hpp>

// clang-format off
#define flags                \
    flag(simulate)           \
    flag(debug_ast)          \
    flag(debug_ir)           \
    flag(debug_optimized_ir) \
    flag(debug_show_execs)

// clang-format on

std::shared_ptr<Settings> read_settings(int arg_count, const char * const * args) {

    auto settings = std::make_shared<Settings>();

// clang-format off
#define flag(name) auto name = false;
    flags
#undef flag

    auto print_help = false;
    auto print_version = false;
	auto debug = false;
    // clang-format on

    auto cli = lyra::help(print_help) | lyra::opt(print_version)["--version"]["-V"]
             | lyra::opt(simulate)["--sim"]["--simulate"] | lyra::opt(debug_ast)["--ast"]
             | lyra::opt(debug_ir)["--llvm"] | lyra::opt(debug_optimized_ir)["--opt-llvm"]
             | lyra::opt(debug)["--debug"] | lyra::opt(debug_show_execs)["--exec"]
             | lyra::arg(settings->file_to_read, "file to read");

    if (auto result = cli.parse({arg_count, args}); not result) {
        std::cerr << "Error in command line : " << result.errorMessage() << std::endl;
        exit(1);
    }

    if (print_help) {
        std::cout << cli << std::endl;
        exit(0);
    }

    if (print_version) {
        std::cout << *args << "\nVersion: " << version::full << std::endl;
        exit(0);
    }

// clang-format off
#define flag(name) if(name) { settings->set_flag(cmd_flag::name); }
	flags
#undef flag

	if(debug){
		settings->set_flag(cmd_flag::debug_ast);
		settings->set_flag(cmd_flag::debug_ir);
		settings->set_flag(cmd_flag::debug_optimized_ir);
		settings->set_flag(cmd_flag::debug_show_execs);
	}

    // clang-format on

    return settings;
}
