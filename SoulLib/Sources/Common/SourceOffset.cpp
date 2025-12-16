#include "Common/SourceOffset.h"

namespace Soul
{
	Bool8 SourceOffset::Equals(const SourceOffset lhs, const SourceOffset rhs) noexcept
	{
		return lhs.begin == rhs.begin && lhs.end == rhs.end;
	}
}  // namespace Soul
