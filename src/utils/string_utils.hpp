#ifndef STRING_UTILS_HPP
#define STRING_UTILS_HPP

#include <filesystem>
#include <string>

std::string unquote(const std::string & /*input*/);
std::filesystem::path normalized_absolute_path(const std::string & /*input*/);

#endif
