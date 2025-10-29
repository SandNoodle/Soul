#pragma once

#include "common/compile_time_dictionary.h"
#include "common/types/type.h"
#include "core/types.h"

#include <concepts>
#include <sstream>
#include <string>
#include <type_traits>
#include <variant>

namespace soul
{
	class Value;
	std::partial_ordering operator<=>(const Value& lhs, const Value& rhs) noexcept;

	namespace detail
	{
		template <types::PrimitiveType::Kind V>
		using PrimitiveKindToValueType
			= VTDictionary<V,
		                   VTTranslation<types::PrimitiveType::Kind::Boolean, bool>,
		                   VTTranslation<types::PrimitiveType::Kind::Char, char>,
		                   VTTranslation<types::PrimitiveType::Kind::Float32, f32>,
		                   VTTranslation<types::PrimitiveType::Kind::Float64, f64>,
		                   VTTranslation<types::PrimitiveType::Kind::Int32, i32>,
		                   VTTranslation<types::PrimitiveType::Kind::Int64, i64>,
		                   VTTranslation<types::PrimitiveType::Kind::String, std::string>>::Type;

		template <types::PrimitiveType::Kind V>
		using PrimitiveKindToViewType
			= VTDictionary<V,
		                   VTTranslation<types::PrimitiveType::Kind::Boolean, bool>,
		                   VTTranslation<types::PrimitiveType::Kind::Char, char>,
		                   VTTranslation<types::PrimitiveType::Kind::Float32, f32>,
		                   VTTranslation<types::PrimitiveType::Kind::Float64, f64>,
		                   VTTranslation<types::PrimitiveType::Kind::Int32, i32>,
		                   VTTranslation<types::PrimitiveType::Kind::Int64, i64>,
		                   VTTranslation<types::PrimitiveType::Kind::String, std::string_view>>::Type;

		template <types::PrimitiveType::Kind V>
		class ValueBase
		{
			public:
			using ValueType = PrimitiveKindToValueType<V>;
			using ViewType  = PrimitiveKindToViewType<V>;

			private:
			ValueType _value;

			public:
			ValueBase(ViewType value) noexcept : _value(ValueType(std::move(value))) {}

			constexpr bool operator==(const ValueBase& other) const noexcept  = default;
			constexpr auto operator<=>(const ValueBase& other) const noexcept = default;
			constexpr      operator ViewType() const noexcept { return static_cast<ViewType>(_value); }
		};

		template <types::PrimitiveType::Kind... Vs>
		class ScalarBase
		{
			public:
			template <types::PrimitiveType::Kind V>
			using ValueType = ValueBase<V>;

			private:
			std::variant<ValueBase<Vs>...> _value;

			public:
			template <types::PrimitiveType::Kind V>
			ScalarBase(ValueType<V> value) : _value(std::move(value))
			{
			}
			constexpr bool operator==(const ScalarBase& other) const noexcept  = default;
			constexpr auto operator<=>(const ScalarBase& other) const noexcept = default;
			explicit       operator std::string() const;

			template <types::PrimitiveType::Kind V, typename... Args>
			static constexpr ValueType<V> create(Args&&... args)
			{
				return ValueType<V>(std::forward<Args>(args)...);
			}

			template <types::PrimitiveType::Kind V>
			constexpr bool is() const noexcept
			{
				return std::holds_alternative<ValueBase<V>>(_value);
			}

			template <types::PrimitiveType::Kind V>
			constexpr ValueBase<V>::ViewType as() const noexcept
			{
				return std::get<ValueBase<V>>(_value);
			}
		};
	}  // namespace detail

	using Scalar = detail::ScalarBase<types::PrimitiveType::Kind::Boolean,
	                                  types::PrimitiveType::Kind::Char,
	                                  types::PrimitiveType::Kind::Float32,
	                                  types::PrimitiveType::Kind::Float64,
	                                  types::PrimitiveType::Kind::Int32,
	                                  types::PrimitiveType::Kind::Int64,
	                                  types::PrimitiveType::Kind::String>;

	class Identifier
	{
		public:
		using ValueType = std::string;
		using ViewType  = std::string_view;

		private:
		ValueType _value;

		public:
		constexpr Identifier(ViewType value) : _value(std::move(value)) {}
		constexpr bool operator==(const Identifier& other) const noexcept  = default;
		constexpr auto operator<=>(const Identifier& other) const noexcept = default;
		constexpr      operator ViewType() const noexcept { return static_cast<ViewType>(_value); }
		explicit       operator std::string() const;

		static auto create(ViewType value) { return Identifier(std::move(value)); }
	};

	class Array
	{
		public:
		using Values = std::vector<Value>;

		private:
		Values _values;

		public:
		constexpr bool operator==(const Array& other) const noexcept  = default;
		constexpr auto operator<=>(const Array& other) const noexcept = default;
		explicit       operator std::string() const;
	};

	class Struct
	{
		public:
		using Members = std::vector<Value>;

		private:
		Members _members;

		public:
		constexpr bool operator==(const Struct& other) const noexcept  = default;
		constexpr auto operator<=>(const Struct& other) const noexcept = default;
		explicit       operator std::string() const;
	};

	using UnknownValue = std::monostate;

	template <typename T>
	concept ValueKind = std::same_as<T, UnknownValue>  //
	                 || std::same_as<T, Scalar>        //
	                 || std::same_as<T, Array>         //
	                 || std::same_as<T, Struct>        //
	                 || std::same_as<T, Identifier>    //
		;

	/**
	 * @brief Represents a single `value` of a given type in the language.
	 */
	class Value
	{
		public:
		using Variant = std::variant<UnknownValue, Scalar, Array, Struct, Identifier>;

		private:
		Variant _value = UnknownValue{};

		public:
		Value()                      = default;
		Value(const Value&) noexcept = default;
		Value(Value&&) noexcept      = default;
		Value(Scalar value);
		Value(Identifier value);
		Value(Variant value);
		template <types::PrimitiveType::Kind V>
		Value(Scalar::ValueType<V> value) : _value(std::move(value))
		{
		}

		Value&   operator=(const Value&) noexcept        = default;
		Value&   operator=(Value&&) noexcept             = default;
		bool     operator==(const Value&) const noexcept = default;
		explicit operator std::string() const;

		/**
		 * @brief Verifies if Value of a given ValueKind type.
		 * @tparam T Type satisfying the ValueKind concept.
		 * @return \b true if it is, \b false otherwise.
		 */
		template <ValueKind T>
		[[nodiscard]] constexpr bool is() const noexcept
		{
			return std::holds_alternative<T>(_value);
		}

		/**
		 * @brief Returns the underlying value (of a given type).
		 * @important Does not perform any validation - assumes that Value::is<T> was used first.
		 * @tparam T Type satisfying the ValueKind concept.
		 */
		template <ValueKind T>
		[[nodiscard]] constexpr const T& as() const noexcept
		{
			return std::get<T>(_value);
		}

		/**
		 * @brief Returns the underlying value (of a given type).
		 * @important Does not perform any validation - assumes that Value::is<T> was used first.
		 * @tparam T Type satisfying the ValueKind concept.
		 */
		template <ValueKind T>
		[[nodiscard]] constexpr T& as() noexcept
		{
			return std::get<T>(_value);
		}

		friend std::partial_ordering operator<=>(const Value& lhs, const Value& rhs) noexcept;
	};
}  // namespace soul
