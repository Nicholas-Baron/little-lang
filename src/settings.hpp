#ifndef SETTINGS_HPP
#define SETTINGS_HPP

#include <memory>
#include <string>
#include <vector>

// Some command line flags can be stored in a bitfield.
enum class cmd_flag : unsigned {
    simulate = 1 << 0,
    debug_ast = 1 << 1,
    debug_ir = 1 << 2,
    debug_optimized_ir = 1 << 3,
    debug_show_execs = 1 << 4,
    run_result = 1 << 5,
    no_output = 1 << 6,
};

struct Settings {
    std::string file_to_read;

    [[nodiscard]] bool flag_is_set(cmd_flag flag) const {
        return (flags & static_cast<unsigned>(flag)) != 0;
    }

    void set_flag(cmd_flag flag) { flags |= static_cast<unsigned>(flag); }

    unsigned flags{};

    std::vector<std::string> extra_args;
};

// Parse all CLI arguments before the "--".
// This allows `littlec myfile.lil -- --myflag` to pass `--myflag` to the resulting executable.
std::shared_ptr<Settings> read_settings(int arg_count, const char * const * args);

#endif
