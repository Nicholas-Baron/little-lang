#ifndef LOCATION_HPP
#define LOCATION_HPP

#include <iosfwd>
#include <utility>

class Location final {

  public:
    constexpr Location()
        : Location(0, 0, 0, 0) {}

    explicit constexpr Location(int start_line, int start_col, int end_line, int end_col)
        : start_pos(start_line, start_col)
        , end_pos(end_line, end_col) {}

    [[nodiscard]] constexpr auto first_line() const noexcept { return start_pos.first; }
    [[nodiscard]] constexpr auto first_column() const noexcept { return start_pos.second; }
    [[nodiscard]] constexpr auto last_line() const noexcept { return end_pos.first; }
    [[nodiscard]] constexpr auto last_column() const noexcept { return end_pos.second; }

    [[nodiscard]] constexpr bool operator==(const Location & rhs) const noexcept {
        return start_pos == rhs.start_pos and end_pos == rhs.end_pos;
    }

    [[nodiscard]] constexpr bool oneline() const noexcept {
        return start_pos.first == end_pos.first;
    }

  private:
    // Line first
    std::pair<int, int> start_pos;
    std::pair<int, int> end_pos;
};

constexpr bool operator!=(const Location & lhs, const Location & rhs) noexcept {
    return not(lhs == rhs);
}

constexpr bool operator<(const Location & lhs, const Location & rhs) noexcept {

    if (lhs.first_line() != rhs.first_line()) { return lhs.first_line() < rhs.first_line(); }

    if (lhs.first_column() != rhs.first_column()) {
        return lhs.first_column() < rhs.first_column();
    }

    if (lhs.last_line() != rhs.last_line()) { return lhs.last_line() < rhs.last_line(); }

    return lhs.last_column() < rhs.last_column();
}

std::ostream & operator<<(std::ostream & lhs, const Location & rhs);

#endif
