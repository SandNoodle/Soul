#pragma once

#include "Core/Types.h"

#include <limits>
#include <memory>
#include <string>
#include <variant>

namespace Soul::Parser
{
	class ArrayTypeSpecifier;
	class BaseTypeSpecifier;
	class PointerTypeSpecifier;
	class TypeSpecifier;

	template <typename T>
	concept TypeSpecifierKind = std::same_as<T, ArrayTypeSpecifier>    //
	                         || std::same_as<T, BaseTypeSpecifier>     //
	                         || std::same_as<T, PointerTypeSpecifier>  //
		;

	constexpr std::strong_ordering operator<=>(const TypeSpecifier& lhs, const TypeSpecifier& rhs);

	class BaseTypeSpecifier
	{
		public:
		using ValueType = std::string;
		using ViewType  = std::string_view;

		private:
		ValueType _value;

		public:
		BaseTypeSpecifier(ViewType value);

		bool operator==(const BaseTypeSpecifier&) const noexcept         = default;
		std::strong_ordering operator<=>(const BaseTypeSpecifier&) const = default;
		explicit operator std::string() const;
	};

	class PointerTypeSpecifier
	{
		private:
		std::unique_ptr<TypeSpecifier> _value;

		public:
		PointerTypeSpecifier(TypeSpecifier&& value);
		PointerTypeSpecifier(const PointerTypeSpecifier&) noexcept;
		PointerTypeSpecifier(PointerTypeSpecifier&&) noexcept;

		PointerTypeSpecifier& operator=(const PointerTypeSpecifier&) noexcept;
		PointerTypeSpecifier& operator=(PointerTypeSpecifier&&) noexcept;
		bool operator==(const PointerTypeSpecifier&) const noexcept = default;
		std::strong_ordering operator<=>(const PointerTypeSpecifier&) const;
		explicit operator std::string() const;
	};

	class ArrayTypeSpecifier
	{
		public:
		using Size = UInt64;

		static constexpr Size k_unbound_size = std::numeric_limits<Size>::max();

		private:
		std::unique_ptr<TypeSpecifier> _value;
		Size _size{ k_unbound_size };

		public:
		ArrayTypeSpecifier(TypeSpecifier&& value, Size capacity);
		ArrayTypeSpecifier(const ArrayTypeSpecifier&) noexcept;
		ArrayTypeSpecifier(ArrayTypeSpecifier&&) noexcept;
		ArrayTypeSpecifier& operator=(const ArrayTypeSpecifier&) noexcept;
		ArrayTypeSpecifier& operator=(ArrayTypeSpecifier&&) noexcept;
		bool operator==(const ArrayTypeSpecifier&) const noexcept = default;
		std::strong_ordering operator<=>(const ArrayTypeSpecifier&) const;
		explicit operator std::string() const;
	};

	/**
	 * @brief
	 */
	class TypeSpecifier
	{
		public:
		using Variant = std::variant<std::monostate, BaseTypeSpecifier, PointerTypeSpecifier, ArrayTypeSpecifier>;

		private:
		Variant _value{};

		public:
		constexpr explicit TypeSpecifier(Variant value) : _value(std::move(value)) {}
		constexpr TypeSpecifier(const BaseTypeSpecifier& other) : _value(other) {}
		constexpr TypeSpecifier(const TypeSpecifier&) noexcept = default;
		constexpr TypeSpecifier(TypeSpecifier&&) noexcept      = default;

		TypeSpecifier& operator=(const TypeSpecifier&) noexcept        = default;
		TypeSpecifier& operator=(TypeSpecifier&&) noexcept             = default;
		constexpr bool operator==(const TypeSpecifier&) const noexcept = default;
		explicit operator std::string() const;

		template <TypeSpecifierKind T>
		[[nodiscard]] constexpr bool Is() const noexcept
		{
			return std::holds_alternative<T>(_value);
		}

		template <TypeSpecifierKind T>
		[[nodiscard]] constexpr const T& As() const noexcept
		{
			return std::holds_alternative<T>(_value);
		}

		friend constexpr std::strong_ordering operator<=>(const TypeSpecifier&, const TypeSpecifier&);
		friend std::hash<TypeSpecifier>;
	};

	constexpr std::strong_ordering operator<=>(const TypeSpecifier& lhs, const TypeSpecifier& rhs)
	{
		return std::tie(lhs._value) <=> std::tie(rhs._value);
	}

	extern const BaseTypeSpecifier k_base_specifier_i8;
	extern const BaseTypeSpecifier k_base_specifier_i16;
	extern const BaseTypeSpecifier k_base_specifier_i32;
	extern const BaseTypeSpecifier k_base_specifier_i64;
	extern const BaseTypeSpecifier k_base_specifier_i128;

	extern const BaseTypeSpecifier k_base_specifier_u8;
	extern const BaseTypeSpecifier k_base_specifier_u16;
	extern const BaseTypeSpecifier k_base_specifier_u32;
	extern const BaseTypeSpecifier k_base_specifier_u64;
	extern const BaseTypeSpecifier k_base_specifier_u128;

	extern const BaseTypeSpecifier k_base_specifier_f16;
	extern const BaseTypeSpecifier k_base_specifier_f32;
	extern const BaseTypeSpecifier k_base_specifier_f64;

	extern const BaseTypeSpecifier k_base_specifier_chr;
	extern const BaseTypeSpecifier k_base_specifier_str;

	extern const BaseTypeSpecifier k_base_specifier_bool;
	extern const BaseTypeSpecifier k_base_specifier_isize;
	extern const BaseTypeSpecifier k_base_specifier_usize;

	extern const BaseTypeSpecifier k_base_specifier_void;
}  // namespace Soul::Parser
