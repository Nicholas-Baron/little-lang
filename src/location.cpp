#include "location.hpp"

#include <iostream>

std::ostream & operator<<(std::ostream & lhs, const Location & rhs) {
    if (rhs.oneline()) {
        return lhs << "Line " << rhs.first_line() << ", Columns " << rhs.first_column() << " to "
                   << rhs.last_column();
    }
    return lhs << "Lines " << rhs.first_line() << " thru " << rhs.last_line();
}
