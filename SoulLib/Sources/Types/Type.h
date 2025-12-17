#pragma once

#include "Core/Types.h"
#include "Types/TypeFwd.h"

#include <concepts>
#include <memory>
#include <ostream>
#include <variant>
#include <vector>

namespace Soul::Types
{
	/** @brief TypeKind is a concept that specifies types present in the language's type system. */
	template <typename T>
	concept TypeKind = std::same_as<T, PrimitiveType>  //
	                || std::same_as<T, PointerType>    //
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
		enum class Kind : UInt8
		{
			UNKNOWN,

			BOOLEAN,

			INT8,
			INT16,
			INT32,
			INT64,
			INT128,

			UINT8,
			UINT16,
			UINT32,
			UINT64,
			UINT128,

			FLOAT16,
			FLOAT32,
			FLOAT64,

			CHAR,
			STRING,

			ISIZE,
			USIZE,

			VOID,
		};

		public:
		Kind type = Kind::UNKNOWN;

		public:
		constexpr PrimitiveType(Kind type = Kind::UNKNOWN) : type(type) {}

		bool operator==(const PrimitiveType&) const noexcept         = default;
		std::strong_ordering operator<=>(const PrimitiveType&) const = default;
		explicit operator std::string() const;

		friend std::ostream& operator<<(std::ostream& os, const PrimitiveType&);
	};

	/**
	 * @brief Represents a memory pointer of a given type.
	 */
	class PointerType
	{
		private:
		std::unique_ptr<Type> _type;

		public:
		PointerType(const Type& type);
		PointerType(const PointerType&) noexcept;
		PointerType(PointerType&&) noexcept;

		PointerType& operator=(const PointerType&) noexcept;
		PointerType& operator=(PointerType&&) noexcept;
		bool operator==(const PointerType&) const noexcept = default;
		std::strong_ordering operator<=>(const PointerType&) const;
		explicit operator std::string() const;

		const Type& DataType() const noexcept;

		friend std::ostream& operator<<(std::ostream& os, const StructType& type);
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

		const Type& DataType() const noexcept;

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
		using Variant = std::variant<PrimitiveType, PointerType, ArrayType, StructType>;

		private:
		Variant _type = PrimitiveType::Kind::UNKNOWN;

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
		[[nodiscard]] constexpr bool Is() const noexcept
		{
			return std::holds_alternative<T>(_type);
		}

		/**
		 * @brief Returns the requested underlying type.
		 * @important Does not perform any validation - assumes that Type::is<T> was used first.
		 */
		template <TypeKind T>
		[[nodiscard]] constexpr const T& As() const noexcept
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
}  // namespace Soul::Types
