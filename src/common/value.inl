#pragma once

namespace soul
{
	namespace detail
	{
		template <types::PrimitiveType::Kind V>
		constexpr ValueBase<V>::ValueBase(ViewType value) noexcept : _value(static_cast<ValueType>(std::move(value)))
		{
		}

		template <types::PrimitiveType::Kind V>
		constexpr ValueBase<V>::operator ViewType() const noexcept
		{
			return static_cast<ViewType>(_value);
		}

		template <types::PrimitiveType::Kind V>
		constexpr auto ValueBase<V>::type() const noexcept -> types::Type
		{
			return V;
		}

		template <types::PrimitiveType::Kind... Vs>
		template <types::PrimitiveType::Kind V>
		constexpr ScalarBase<Vs...>::ScalarBase(ValueType<V> value) : _value(std::move(value))
		{
		}

		template <types::PrimitiveType::Kind... Vs>
		template <types::PrimitiveType::Kind V, typename... Args>
		constexpr auto ScalarBase<Vs...>::create(Args&&... args) -> ValueType<V>
		{
			return ValueType<V>(std::forward<Args>(args)...);
		}

		template <types::PrimitiveType::Kind... Vs>
		template <types::PrimitiveType::Kind V>
		constexpr auto ScalarBase<Vs...>::is() const noexcept -> bool
		{
			return std::holds_alternative<ValueBase<V>>(_value);
		}

		template <types::PrimitiveType::Kind... Vs>
		template <types::PrimitiveType::Kind V>
		constexpr auto ScalarBase<Vs...>::as() const noexcept -> ValueBase<V>::ViewType
		{
			return std::get<ValueBase<V>>(_value);
		}
	}  // namespace detail

	constexpr Identifier::Identifier(ViewType value) : _value(std::move(value)) {}

	constexpr Array::Array(Values values) : _values(std::move(values)) {}

	constexpr Struct::Struct(Members members) : _members(std::move(members)) {}

	constexpr Value::Value(Identifier value) : _value(std::move(value)) {}

	constexpr Value::Value(Variant value) : _value(std::move(value)) {}

	template <types::PrimitiveType::Kind V>
	constexpr Value::Value(Scalar::ValueType<V> value) : _value(std::move(value))
	{
	}

	template <ValueKind T>
	constexpr auto Value::is() const noexcept -> bool
	{
		return std::holds_alternative<T>(_value);
	}

	template <ValueKind T>
	constexpr auto Value::as() const noexcept -> const T&
	{
		return std::get<T>(_value);
	}
}  // namespace soul
