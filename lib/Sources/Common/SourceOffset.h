#pragma once

#include "Soul.h"

namespace Soul
{
	/**
	 * @brief Represents some relative position into a source file.
	 */
	struct SourceOffset
	{
		UInt32 begin = 0;
		UInt32 end   = 0;

		static Bool8 Equal(SourceOffset lhs, SourceOffset rhs) noexcept;
	};
}  // namespace Soul
