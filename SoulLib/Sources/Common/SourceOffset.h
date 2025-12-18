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
		UInt32 row    = 1;
		UInt32 column = 0;

		public:
		bool operator==(const SourceOffset& other) const noexcept                  = default;
		std::strong_ordering operator<=>(const SourceOffset& other) const noexcept = default;
		explicit operator std::string() const;
	};

	template <typename T>
	constexpr T& operator<<(T& os, SourceOffset location)
	{
		return os << std::string(location);
	}
}  // namespace Soul
