#pragma once

#include <optional>

namespace Soul
{
	// TODO: Replace with proper implementation.
	template <typename T>
	using Optional                 = std::optional<T>;
	using NoneType                 = std::nullopt_t;
	static constexpr NoneType None = std::nullopt;
}  // namespace Soul
