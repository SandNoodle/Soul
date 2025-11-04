#pragma once

#include "core/types.h"

#include <format>
#include <string>

namespace soul
{
	/**
	 * @brief Represents some relative position into a source file.
	 */
	struct SourceLocation
	{
		public:
		u32 row    = 1;
		u32 column = 0;

		public:
		constexpr bool operator==(SourceLocation other) const noexcept
		{
			return row == other.row && column == other.column;
		}

		constexpr auto operator<=>(SourceLocation other) const noexcept
		{
			return std::tie(row, column) <=> std::tie(other.row, other.column);
		}

		explicit operator std::string() const { return std::format("{}:{}", row, column); }
	};

	template <typename T>
	constexpr T& operator<<(T& os, SourceLocation location)
	{
		return os << std::string(location);
	}
}  // namespace soul
