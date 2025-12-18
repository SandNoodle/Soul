#pragma once
#include "SourceOffset.h"

namespace Soul
{
	SourceOffset::operator std::string() const { return std::format("{}:{}", row, column); }
}  // namespace Soul
