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

	using USize = UInt64;
	using ISize = Int64;

	static_assert(sizeof(Bool8) == 1, "Expected Bool8 to be 1 byte.");
	static_assert(sizeof(Bool16) == 2, "Expected Bool16 to be 2 bytes.");
	static_assert(sizeof(Bool32) == 4, "Expected Bool32 to be 4 bytes.");
	static_assert(sizeof(Bool64) == 8, "Expected Bool64 to be 8 bytes.");

	static_assert(sizeof(UInt8) == 1, "Expected UInt8 to be 1 byte.");
	static_assert(sizeof(UInt16) == 2, "Expected UInt16 to be 2 bytes.");
	static_assert(sizeof(UInt32) == 4, "Expected UInt32 to be 4 bytes.");
	static_assert(sizeof(UInt64) == 8, "Expected UInt64 to be 8 bytes.");

	static_assert(sizeof(Int8) == 1, "Expected Int8 to be 1 byte.");
	static_assert(sizeof(Int16) == 2, "Expected Int16 to be 2 bytes.");
	static_assert(sizeof(Int32) == 4, "Expected Int32 to be 4 bytes.");
	static_assert(sizeof(Int64) == 8, "Expected Int64 to be 8 bytes.");

	static_assert(sizeof(Float32) == sizeof(UInt32), "Expected Float32 to be 4 bytes.");
	static_assert(sizeof(Float64) == sizeof(UInt64), "Expected Float64 to be 8 bytes.");
}  // namespace Soul
