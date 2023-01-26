#pragma once

#include "literal_type.hpp"
#include "operations.hpp"

#include <string>

// TODO: namespace?

[[nodiscard]] std::string token_to_string(operation::binary /*op*/);
[[nodiscard]] std::string token_to_string(operation::unary /*op*/);
[[nodiscard]] std::string token_to_string(literal_type /*op*/);
