#ifndef LOCATION_HPP
#define LOCATION_HPP

#include <iosfwd>
#include <utility>

class Location final {

  public:
    constexpr Location()
        : Location(0, 0) {}

    constexpr Location(int line, int column)
        : line_num{line}
        , column_num{column} {}

    [[nodiscard]] constexpr auto line() const noexcept { return line_num; }
    [[nodiscard]] constexpr auto column() const noexcept { return column_num; }

  private:
    friend constexpr bool operator==(const Location & lhs, const Location & rhs) noexcept {
        return lhs.line() == rhs.line() and lhs.column() == rhs.column();
    }

    friend constexpr bool operator!=(const Location & lhs, const Location & rhs) noexcept {
        return not(lhs == rhs);
    }

    friend constexpr bool operator<(const Location & lhs, const Location & rhs) noexcept {

        if (lhs.line() != rhs.line()) { return lhs.line() < rhs.line(); }

        return lhs.column() < rhs.column();
    }

    int line_num;
    int column_num;
};

std::ostream & operator<<(std::ostream & lhs, const Location & rhs);

#endif
