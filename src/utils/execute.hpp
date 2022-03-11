#ifndef EXECUTE_HPP
#define EXECUTE_HPP

#include <string>
#include <vector>

// Returns `false` on any error.
[[nodiscard]] bool exec_command(std::vector<std::string> && cmd, bool debug);

#endif
