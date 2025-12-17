#pragma once

#include <cstddef>
#include <cstdint>

namespace Soul
{
	using Bool8  = std::uint8_t;
	using Bool16 = std::uint16_t;
	using Bool32 = std::uint32_t;
	using Bool64 = std::uint64_t;

	using UInt8  = std::uint8_t;
	using UInt16 = std::uint16_t;
	using UInt32 = std::uint32_t;
	using UInt64 = std::uint64_t;

	using Int8  = std::int8_t;
	using Int16 = std::int16_t;
	using Int32 = std::int32_t;
	using Int64 = std::int64_t;

	using Float32 = float;
	using Float64 = double;
	static_assert(sizeof(Float32) == sizeof(UInt32), "expected f32 to be 4 bytes.");
	static_assert(sizeof(Float64) == sizeof(UInt64), "expected f64 to be 4 bytes.");
}  // namespace Soul
