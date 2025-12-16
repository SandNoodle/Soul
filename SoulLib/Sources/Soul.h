#pragma once

#include <stdint.h>

// ---- [ Platform ] ----
#define SOUL_PLATFORM_WINDOWS 1  // TODO Move to CMakeLists.txt
#if defined(SOUL_PLATFORM_WINDOWS)

#ifndef _WIN64
#error "Only 64-bit Windows is supported."
#endif

#elif defined(SOUL_PLATFORM_LINUX)
#elif defined(SOUL_PLATFORM_MAC)
#error "Mac OS is not supported."

#else
#error "Unkown platform detected."
#endif

// ---- [ Compiler detection ] ----
#if defined(__clang__)
#define SOUL_COMPILER_CLANG 1

#define SOUL_FORCE_PACKED __attribute__((packed))
#define SOUL_FORCE_INLINE __attribute__((always_inline))
#define SOUL_NEVER_INLINE __attribute__((noinline))

#elif defined(__GNUC__) || defined(__GNUG__)
#define SOUL_COMPILER_GCC 1

#define SOUL_FORCE_PACKED __attribute__((packed))
#define SOUL_FORCE_INLINE __attribute__((always_inline))
#define SOUL_NEVER_INLINE __attribute__((noinline))

#elif defined(_MSC_VER)
#error "MSVC is not supported."

#else
#error "Unknown compiler detected."
#endif

// ---- [ Dynamic Linking ]  ----
// Export
#ifdef SOUL_EXPORT
#if defined(SOUL_COMPILER_CLANG) || defined(SOUL_COMPILER_GCC)
#define SOUL_API __attribute__((visibility("default")))
#else
#error "Cannot define SOUL_API, because compiler is unknown."
#endif

// Import
#else
#if defined(SOUL_COMPILER_CLANG) || defined(SOUL_COMPILER_GCC)
#define SOUL_API
#else
#error "Cannot define SOUL_API, because compiler is unknown."
#endif
#endif

// ---- [ Units ] ----
#define SOUL_GIGABYTES(x) ((x) * 1024ULL * 1024ULL * 1024ULL)
#define SOUL_MEGABYTES(x) ((x) * 1024ULL * 1024ULL)
#define SOUL_KILOBYTES(x) ((x) * 1024ULL)
#define SOUL_BYTES(x) ((x))

// ---- [ Miscellaneous ] ----
#define SOUL_MIN(x, y) ((x) < (y) ? (x) : (y))
#define SOUL_MAX(x, y) ((x) > (y) ? (x) : (y))
#define SOUL_CLAMP(v, min, max) (((v) <= (min)) ? (min) : ((v) >= (max)) ? (max) : (v))

#ifndef SOUL_DEFAULT_ALIGNMENT
#define SOUL_DEFAULT_ALIGNMENT (2 * sizeof(void*))
#endif

namespace Soul
{
	using Bool8  = uint8_t;
	using Bool16 = uint16_t;
	using Bool32 = uint32_t;
	using Bool64 = uint64_t;

	using UInt8  = uint8_t;
	using UInt16 = uint16_t;
	using UInt32 = uint32_t;
	using UInt64 = uint64_t;

	using Int8  = int8_t;
	using Int16 = int16_t;
	using Int32 = int32_t;
	using Int64 = int64_t;

	using Float32 = float;
	using Float64 = double;

	using USize = UInt64;
	using ISize = Int64;

#if defined(SOUL_COMPILER_CLANG) || defined(SOUL_COMPILER_GCC)
	using UInt128 = __uint128_t;
	using Int128  = __int128_t;
#else
#error "Cannot declare 128 bit integer types."
#endif

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

	static_assert(sizeof(Float32) == 4, "Expected Float32 to be 4 bytes.");
	static_assert(sizeof(Float64) == 8, "Expected Float64 to be 8 bytes.");
}  // namespace Soul
