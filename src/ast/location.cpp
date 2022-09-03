#include "location.hpp"

#include <iostream>

std::ostream & operator<<(std::ostream & lhs, const Location & rhs) {
    return lhs << rhs.line() << ":" << rhs.column();
}
