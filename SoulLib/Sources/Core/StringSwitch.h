#pragma once

#include "Core/Optional.h"
#include "Core/StringView.h"

namespace Soul
{
	template <typename T, typename R = T>
	class StringSwitch
	{
		public:
		using ValueType  = T;
		using ReturnType = R;

		private:
		StringView _value;
		Optional<T> _result;

		public:
		SOUL_FORCE_INLINE explicit constexpr StringSwitch(StringView value) noexcept;

		[[nodiscard]] constexpr StringSwitch& Case(StringView c, ValueType value) noexcept;
		[[nodiscard]] constexpr ReturnType Default(ValueType value) noexcept;
	};

	template <typename T, typename R>
	SOUL_FORCE_INLINE constexpr StringSwitch<T, R>::StringSwitch(StringView value) noexcept
		: _value(value), _result(None)
	{
	}

	template <typename T, typename R>
	constexpr auto StringSwitch<T, R>::Case(StringView c, ValueType value) noexcept -> StringSwitch&
	{
		if (_result) {
			return *this;
		}
		if (!StringView::Equals(_value, c)) {
			return *this;
		}
		_result = std::move(value);
		return *this;
	}

	template <typename T, typename R>
	constexpr auto StringSwitch<T, R>::Default(ValueType value) noexcept -> ReturnType
	{
		if (_result) {
			return std::move(*_result);
		}
		return value;
	}
}  // namespace Soul
