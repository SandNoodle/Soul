#pragma once

#include "Common/CompileTimeDictionary.h"
#include "Core/Types.h"
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
		                   VTTranslation<Types::PrimitiveType::Kind::BOOLEAN, bool>,
		                   VTTranslation<Types::PrimitiveType::Kind::CHAR, char>,
		                   VTTranslation<Types::PrimitiveType::Kind::FLOAT32, Float32>,
		                   VTTranslation<Types::PrimitiveType::Kind::FLOAT64, Float64>,
		                   VTTranslation<Types::PrimitiveType::Kind::INT32, Int32>,
		                   VTTranslation<Types::PrimitiveType::Kind::INT64, Int64>,
		                   VTTranslation<Types::PrimitiveType::Kind::STRING, std::string>>::Type;

		template <Types::PrimitiveType::Kind V>
		using PrimitiveKindToViewType
			= VTDictionary<V,
		                   VTTranslation<Types::PrimitiveType::Kind::BOOLEAN, bool>,
		                   VTTranslation<Types::PrimitiveType::Kind::CHAR, char>,
		                   VTTranslation<Types::PrimitiveType::Kind::FLOAT32, Float32>,
		                   VTTranslation<Types::PrimitiveType::Kind::FLOAT64, Float64>,
		                   VTTranslation<Types::PrimitiveType::Kind::INT32, Int32>,
		                   VTTranslation<Types::PrimitiveType::Kind::INT64, Int64>,
		                   VTTranslation<Types::PrimitiveType::Kind::STRING, std::string_view>>::Type;

		template <Types::PrimitiveType::Kind V>
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

			[[nodiscard]] constexpr Types::Type type() const noexcept;
		};

		template <Types::PrimitiveType::Kind... Vs>
		class ScalarBase
		{
			public:
			template <Types::PrimitiveType::Kind V>
			using ValueType = ValueBase<V>;

			private:
			std::variant<ValueBase<Vs>...> _value;

			public:
			template <Types::PrimitiveType::Kind V>
			constexpr ScalarBase(ValueType<V> value);
			constexpr bool operator==(const ScalarBase& other) const noexcept  = default;
			constexpr auto operator<=>(const ScalarBase& other) const noexcept = default;
			explicit operator std::string() const;

			template <Types::PrimitiveType::Kind V, typename... Args>
			static constexpr ValueType<V> create(Args&&... args);

			/**
			 * @brief Verifies if Scalar of a given type.
			 * @return \b true if it is, \b false otherwise.
			 */
			template <Types::PrimitiveType::Kind V>
			constexpr bool is() const noexcept;

			/**
			 * @brief Returns the underlying value (of a given type).
			 * @important Does not perform any validation - assumes that Value::is<T> was used first.
			 */
			template <Types::PrimitiveType::Kind V>
			constexpr ValueBase<V>::ViewType as() const noexcept;

			[[nodiscard]] Types::Type type() const;
		};
	}  // namespace detail

	/** @brief Represents a value of some primitive type. */
	using Scalar = detail::ScalarBase<Types::PrimitiveType::Kind::BOOLEAN,
	                                  Types::PrimitiveType::Kind::CHAR,
	                                  Types::PrimitiveType::Kind::FLOAT32,
	                                  Types::PrimitiveType::Kind::FLOAT64,
	                                  Types::PrimitiveType::Kind::INT32,
	                                  Types::PrimitiveType::Kind::INT64,
	                                  Types::PrimitiveType::Kind::STRING>;

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

		[[nodiscard]] Types::Type type() const noexcept;
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

		[[nodiscard]] Types::Type type() const;
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

		[[nodiscard]] Types::Type type() const;
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
		template <Types::PrimitiveType::Kind V>
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

		[[nodiscard]] Types::Type type() const;

		friend std::partial_ordering operator<=>(const Value& lhs, const Value& rhs);
	};
}  // namespace Soul
#include "Common/Value.inl"
