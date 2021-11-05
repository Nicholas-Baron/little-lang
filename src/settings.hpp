#ifndef _SETTINGS_HPP
#define _SETTINGS_HPP

#include <memory>
#include <string>

struct Settings {
    std::string file_to_read;
    bool print_version{false};
    bool simulate;
};

std::shared_ptr<Settings> read_settings(int arg_count, const char * const * args);

#endif
