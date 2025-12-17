#pragma once

#include "Core/Types.h"

#include <format>
#include <string>

namespace Soul
{
	/**
	 * @brief Represents some relative position into a source file.
	 */
	struct SourceOffset
	{
		public:
		u32 row    = 1;
		u32 column = 0;

		public:
		constexpr bool operator==(SourceOffset other) const noexcept
		{
			return row == other.row && column == other.column;
		}

		constexpr auto operator<=>(SourceOffset other) const noexcept
		{
			return std::tie(row, column) <=> std::tie(other.row, other.column);
		}

		explicit operator std::string() const { return std::format("{}:{}", row, column); }
	};

	template <typename T>
	constexpr T& operator<<(T& os, SourceOffset location)
	{
		return os << std::string(location);
	}
}  // namespace Soul
