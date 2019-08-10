#ifndef _LOCATION_HPP
#define _LOCATION_HPP

#include <utility>

class Location final {

   public:
	explicit Location(int start_line, int start_col, int end_line, int end_col)
		: start_pos(start_line, start_col), end_pos(end_line, end_col) {}

	[[nodiscard]] auto first_line() const noexcept { return start_pos.first; }
	[[nodiscard]] auto first_column() const noexcept {
		return start_pos.second;
	}
	[[nodiscard]] auto last_line() const noexcept { return end_pos.first; }
	[[nodiscard]] auto last_column() const noexcept { return end_pos.second; }

   private:
	// Line first
	std::pair<int, int> start_pos;
	std::pair<int, int> end_pos;
};

#endif
