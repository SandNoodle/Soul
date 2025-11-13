#pragma once

#include "common/types/types_fwd.h"
#include "core/types.h"

#include <concepts>
#include <memory>
#include <ostream>
#include <variant>
#include <vector>

namespace soul::types
{
	/** @brief TypeKind is a concept that specifies types present in the language's type system. */
	template <typename T>
	concept TypeKind = std::same_as<T, PrimitiveType>  //
	                || std::same_as<T, ArrayType>      //
	                || std::same_as<T, StructType>     //
		;

	constexpr std::strong_ordering operator<=>(const Type& lhs, const Type& rhs);

	/**
	 * @brief Represents the most basic data type present in the language.
	 * It's a base from which all the other types are constructed.
	 */
	class PrimitiveType
	{
		public:
		enum class Kind : u8
		{
			Unknown,
			Boolean,
			Char,
			Float32,
			Float64,
			Int32,
			Int64,
			String,
			Void,
		};

		public:
		Kind type = Kind::Unknown;

		public:
		constexpr PrimitiveType(Kind type = Kind::Unknown) : type(type) {}

		bool operator==(const PrimitiveType&) const noexcept         = default;
		std::strong_ordering operator<=>(const PrimitiveType&) const = default;
		explicit operator std::string() const;

		friend std::ostream& operator<<(std::ostream& os, const PrimitiveType&);
	};

	/**
	 * @brief Represents a collection of elements of a given type.
	 */
	class ArrayType
	{
		private:
		std::unique_ptr<Type> _type;

		public:
		ArrayType(const Type& contained_type);
		ArrayType(const ArrayType&) noexcept;
		ArrayType(ArrayType&&) noexcept;

		ArrayType& operator=(const ArrayType&) noexcept;
		ArrayType& operator=(ArrayType&&) noexcept;
		bool operator==(const ArrayType&) const noexcept = default;
		std::strong_ordering operator<=>(const ArrayType&) const;
		explicit operator std::string() const;

		const Type& data_type() const noexcept;

		friend std::ostream& operator<<(std::ostream& os, const ArrayType&);
	};

	/**
	 * @brief Represents a composite data structure that is a collection of (possibly) different data types.
	 */
	class StructType
	{
		public:
		using ContainedTypes = std::vector<Type>;

		public:
		ContainedTypes types;

		public:
		StructType(ContainedTypes types);

		bool operator==(const StructType&) const noexcept = default;
		std::strong_ordering operator<=>(const StructType&) const;
		explicit operator std::string() const;

		friend std::ostream& operator<<(std::ostream& os, const StructType& type);
	};

	/**
	 * @brief Represents specific `type` in the language's type system.
	 * It is capable of describing all builtin, nested and user defined types.
	 */
	class Type
	{
		public:
		using Variant = std::variant<PrimitiveType, ArrayType, StructType>;

		private:
		Variant _type = PrimitiveType::Kind::Unknown;

		public:
		constexpr Type() noexcept            = default;
		constexpr Type(const Type&) noexcept = default;
		constexpr Type(Type&&) noexcept      = default;
		explicit constexpr Type(Variant&& type) noexcept : _type(std::move(type)) {}
		constexpr Type(PrimitiveType::Kind type) : _type(PrimitiveType(type)) {}

		Type& operator=(const Type&) noexcept                 = default;
		Type& operator=(Type&&) noexcept                      = default;
		constexpr bool operator==(const Type&) const noexcept = default;
		explicit operator std::string() const;

		/**
		 * @brief Verifies if a Type is of a given TypeKind's type.
		 * @return \b true if it is, \b false otherwise.
		 */
		template <TypeKind T>
		[[nodiscard]] constexpr bool is() const noexcept
		{
			return std::holds_alternative<T>(_type);
		}

		/**
		 * @brief Returns the requested underlying type.
		 * @important Does not perform any validation - assumes that Type::is<T> was used first.
		 */
		template <TypeKind T>
		[[nodiscard]] constexpr const T& as() const noexcept
		{
			return std::get<T>(_type);
		}

		friend std::ostream& operator<<(std::ostream& os, const Type& type);
		friend constexpr std::strong_ordering operator<=>(const Type&, const Type&);
	};

	constexpr std::strong_ordering operator<=>(const Type& lhs, const Type& rhs)
	{
		return std::tie(lhs._type) <=> std::tie(rhs._type);
	}
}  // namespace soul::types
