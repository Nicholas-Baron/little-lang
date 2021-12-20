#include "string_utils.hpp"

std::string unquote(const std::string & input) {
    // Quoted string has at least the 2 double quotes
    if (input.size() < 2) { return input; }

    // Both ends must be quoted
    if (input.front() != '\"' or input.back() != '\"') { return input; }

    return input.substr(1, input.size() - 2);
}

namespace fs = std::filesystem;
fs::path normalized_absolute_path(const std::string & relative_path) {

    return fs::canonical(fs::current_path() / relative_path);
}
