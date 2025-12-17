#pragma once

#include "Common/CompileTimeDictionary.h"
#include "Soul.h"
#include "Types/Type.h"

#include <concepts>
#include <sstream>
#include <string>
#include <type_traits>
#include <variant>

namespace Soul
{
	class Value;

	std::partial_ordering operator<=>(const Value& lhs, const Value& rhs);

	namespace detail
	{
		template <Types::PrimitiveType::Kind V>
		using PrimitiveKindToValueType
			= VTDictionary<V,
		                   VTTranslation<Types::PrimitiveType::Kind::Boolean, bool>,
		                   VTTranslation<Types::PrimitiveType::Kind::Char, char>,
		                   VTTranslation<Types::PrimitiveType::Kind::Float32, f32>,
		                   VTTranslation<Types::PrimitiveType::Kind::Float64, f64>,
		                   VTTranslation<Types::PrimitiveType::Kind::Int32, i32>,
		                   VTTranslation<Types::PrimitiveType::Kind::Int64, i64>,
		                   VTTranslation<Types::PrimitiveType::Kind::String, std::string>>::Type;

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
			constexpr ValueBase(ViewType value) noexcept;
			constexpr bool operator==(const ValueBase& other) const noexcept  = default;
			constexpr auto operator<=>(const ValueBase& other) const noexcept = default;
			[[nodiscard]] constexpr operator ViewType() const noexcept;

			[[nodiscard]] constexpr types::Type type() const noexcept;
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
			constexpr ScalarBase(ValueType<V> value);
			constexpr bool operator==(const ScalarBase& other) const noexcept  = default;
			constexpr auto operator<=>(const ScalarBase& other) const noexcept = default;
			explicit operator std::string() const;

			template <types::PrimitiveType::Kind V, typename... Args>
			static constexpr ValueType<V> create(Args&&... args);

			/**
			 * @brief Verifies if Scalar of a given type.
			 * @return \b true if it is, \b false otherwise.
			 */
			template <types::PrimitiveType::Kind V>
			constexpr bool is() const noexcept;

			/**
			 * @brief Returns the underlying value (of a given type).
			 * @important Does not perform any validation - assumes that Value::is<T> was used first.
			 */
			template <types::PrimitiveType::Kind V>
			constexpr ValueBase<V>::ViewType as() const noexcept;

			[[nodiscard]] types::Type type() const;
		};
	}  // namespace detail

	/** @brief Represents a value of some primitive type. */
	using Scalar = detail::ScalarBase<types::PrimitiveType::Kind::Boolean,
	                                  types::PrimitiveType::Kind::Char,
	                                  types::PrimitiveType::Kind::Float32,
	                                  types::PrimitiveType::Kind::Float64,
	                                  types::PrimitiveType::Kind::Int32,
	                                  types::PrimitiveType::Kind::Int64,
	                                  types::PrimitiveType::Kind::String>;

	/**
	 * @brief Represents a reference to some variable, function name, etc.
	 */
	class Identifier
	{
		public:
		using ValueType = std::string;
		using ViewType  = std::string_view;

		private:
		ValueType _value;

		public:
		constexpr Identifier(ViewType value);
		constexpr bool operator==(const Identifier& other) const noexcept  = default;
		constexpr auto operator<=>(const Identifier& other) const noexcept = default;
		constexpr operator ViewType() const noexcept;
		explicit operator std::string() const;

		static constexpr Identifier create(ViewType value);

		[[nodiscard]] types::Type type() const noexcept;
	};

	/**
	 * @brief Represents a linear sequence of values.
	 */
	class Array
	{
		public:
		using Values = std::vector<Value>;

		private:
		Values _values;

		public:
		constexpr Array(Values values);
		constexpr bool operator==(const Array& other) const noexcept = default;
		constexpr auto operator<=>(const Array& other) const         = default;
		explicit operator std::string() const;

		[[nodiscard]] types::Type type() const;
	};

	/**
	 * @brief Represents a user-defined structure of values.
	 */
	class Struct
	{
		public:
		using Members = std::vector<Value>;

		private:
		Members _members;

		public:
		constexpr Struct(Members members);
		constexpr bool operator==(const Struct& other) const noexcept = default;
		constexpr auto operator<=>(const Struct& other) const         = default;
		explicit operator std::string() const;

		[[nodiscard]] types::Type type() const;
	};

	/** @brief Represents a value of an unknown type. */
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
		constexpr Value()                      = default;
		constexpr Value(const Value&) noexcept = default;
		constexpr Value(Value&&) noexcept      = default;
		constexpr Value(Identifier value);
		constexpr Value(Variant value);
		template <types::PrimitiveType::Kind V>
		constexpr Value(Scalar::ValueType<V> value);

		constexpr Value& operator=(const Value&) noexcept      = default;
		constexpr Value& operator=(Value&&) noexcept           = default;
		constexpr bool operator==(const Value&) const noexcept = default;
		explicit operator std::string() const;

		static std::string_view internal_name(const Value& value);

		/**
		 * @brief Verifies if Value of a given ValueKind type.
		 * @return \b true if it is, \b false otherwise.
		 */
		template <ValueKind T>
		[[nodiscard]] constexpr bool is() const noexcept;

		/**
		 * @brief Returns the underlying value (of a given type).
		 * @important Does not perform any validation - assumes that Value::is<T> was used first.
		 */
		template <ValueKind T>
		[[nodiscard]] constexpr const T& as() const noexcept;

		[[nodiscard]] types::Type type() const;

		friend std::partial_ordering operator<=>(const Value& lhs, const Value& rhs);
	};
}  // namespace Soul
#include "Common/value.inl"
