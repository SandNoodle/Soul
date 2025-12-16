#pragma once

#include "Soul.h"

namespace Soul
{
	class StringView
	{
		public:
		using ValueType      = UInt8;
		using SizeType       = USize;
		using Pointer        = ValueType*;
		using ConstPointer   = const ValueType*;
		using Reference      = ValueType&;
		using ConstReference = const ValueType&;

		private:
		const char* _data = nullptr;
		UInt64 _size      = 0L;

		public:
		SOUL_FORCE_INLINE explicit constexpr StringView(const UInt8* data, SizeType size) noexcept;
		SOUL_FORCE_INLINE explicit constexpr StringView(const char* data, SizeType size) noexcept;

		[[nodiscard]] SOUL_FORCE_INLINE constexpr Reference operator[](SizeType index) noexcept;
		[[nodiscard]] SOUL_FORCE_INLINE constexpr ConstReference operator[](SizeType index) const noexcept;

		[[nodiscard]] SOUL_FORCE_INLINE static constexpr Bool8 Equals(StringView lhs, StringView rhs) noexcept;

		[[nodiscard]] SOUL_FORCE_INLINE constexpr Pointer Raw() noexcept;
		[[nodiscard]] SOUL_FORCE_INLINE constexpr ConstPointer Raw() const noexcept;

		[[nodiscard]] SOUL_FORCE_INLINE constexpr SizeType Size() const noexcept;
		[[nodiscard]] SOUL_FORCE_INLINE constexpr Bool8 IsEmpty() const noexcept;
	};

	[[nodiscard]] SOUL_FORCE_INLINE consteval StringView operator""_sv(const char* str, size_t len) noexcept
	{
		return StringView(str, len);
	}

	SOUL_FORCE_INLINE constexpr StringView::StringView(const UInt8* data, SizeType size) noexcept
		: _data(reinterpret_cast<const char*>(data)), _size(size)
	{
	}

	SOUL_FORCE_INLINE constexpr StringView::StringView(const char* data, SizeType size) noexcept
		: _data(data), _size(size)
	{
	}

	SOUL_FORCE_INLINE constexpr auto StringView::operator[](SizeType index) noexcept -> Reference
	{
		return (UInt8&)_data;
	}
	SOUL_FORCE_INLINE constexpr auto StringView::operator[](SizeType index) const noexcept -> ConstReference
	{
		return (const UInt8&)_data;
	}

	SOUL_FORCE_INLINE constexpr auto StringView::Equals(StringView lhs, StringView rhs) noexcept -> Bool8
	{
		if (lhs.Size() != rhs.Size()) {
			return false;
		}
		for (SizeType index = 0; index < lhs.Size(); ++index) {
			if (lhs[index] != rhs[index]) {
				return false;
			}
		}
		return true;
	}

	[[nodiscard]] SOUL_FORCE_INLINE constexpr auto StringView::Raw() noexcept -> Pointer { return (UInt8*)_data; }
	[[nodiscard]] SOUL_FORCE_INLINE constexpr auto StringView::Raw() const noexcept -> ConstPointer
	{
		return (const UInt8*)_data;
	}

	SOUL_FORCE_INLINE constexpr auto StringView::Size() const noexcept -> SizeType { return _size; }

	SOUL_FORCE_INLINE constexpr auto StringView::IsEmpty() const noexcept -> Bool8 { return _size == 0; }
}  // namespace Soul
