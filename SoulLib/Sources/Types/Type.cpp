#include "Types/Type.h"

#include <sstream>
#include <unordered_map>

namespace Soul::Types
{
	Type::operator std::string() const
	{
		return std::visit([](const auto& arg) -> std::string { return std::string(arg); }, _type);
	}

	std::ostream& operator<<(std::ostream& os, const Type& type) { return os << std::string(type); }

	PrimitiveType::operator std::string() const
	{
		using namespace std::string_view_literals;
		static constexpr auto k_unknown                           = "__unknown__"sv;
		static std::unordered_map<Kind, std::string_view> k_types = {
			{ Kind::UNKNOWN, k_unknown   },
            { Kind::BOOLEAN, "bool"sv    },
            { Kind::CHAR,    "char"sv    },
			{ Kind::FLOAT32, "float32"sv },
            { Kind::FLOAT64, "float64"sv },
            { Kind::INT32,   "int32"sv   },
			{ Kind::INT64,   "int64"sv   },
            { Kind::STRING,  "string"sv  },
            { Kind::VOID,    "void"sv    },
		};
		if (!k_types.contains(type)) [[unlikely]] {
			return std::string(k_unknown);
		}
		return std::string(k_types.at(type));
	}

	std::ostream& operator<<(std::ostream& os, const PrimitiveType& type) { return os << std::string(type); }

	PointerType::PointerType(const Type& type) : _type(std::make_unique<Type>(type)) {}

	PointerType::PointerType(const PointerType& other) noexcept : _type(std::make_unique<Type>(*other._type.get())) {}

	PointerType::PointerType(PointerType&& other) noexcept : _type(std::make_unique<Type>(*other._type.get())) {}

	PointerType& PointerType::operator=(const PointerType& other) noexcept
	{
		if (this == &other) {
			return *this;
		}
		_type = std::make_unique<Type>(*other._type.get());
		return *this;
	}

	PointerType& PointerType::operator=(PointerType&& other) noexcept
	{
		if (this == &other) {
			return *this;
		}
		*_type = std::move(*other._type.get());
		return *this;
	}

	std::strong_ordering PointerType::operator<=>(const PointerType& other) const { return *_type <=> *other._type; }

	PointerType::operator std::string() const { return "*" + std::string(*_type); }

	const Type& PointerType::DataType() const noexcept { return *_type; }

	std::ostream& operator<<(std::ostream& os, const PointerType& type) { return os << std::string(type); }

	ArrayType::ArrayType(const Type& contained_type) : _type(std::make_unique<Type>(contained_type)) {}

	ArrayType::ArrayType(const ArrayType& other) noexcept : _type(std::make_unique<Type>(*other._type.get())) {}

	ArrayType::ArrayType(ArrayType&& other) noexcept : _type(std::make_unique<Type>(*other._type.get())) {}

	ArrayType& ArrayType::operator=(const ArrayType& other) noexcept
	{
		if (this == &other) {
			return *this;
		}
		_type = std::make_unique<Type>(*other._type.get());
		return *this;
	}

	ArrayType& ArrayType::operator=(ArrayType&& other) noexcept
	{
		if (this == &other) {
			return *this;
		}
		*_type = std::move(*other._type.get());
		return *this;
	}

	std::strong_ordering ArrayType::operator<=>(const ArrayType& other) const { return *_type <=> *other._type; }

	ArrayType::operator std::string() const { return std::string(*_type) + "[]"; }

	const Type& ArrayType::DataType() const noexcept { return *_type; }

	std::ostream& operator<<(std::ostream& os, const ArrayType& type) { return os << std::string(type); }

	StructType::StructType(ContainedTypes types) : types(std::move(types)) {}

	std::strong_ordering StructType::operator<=>(const StructType& other) const
	{
		return std::lexicographical_compare_three_way(std::begin(types),
		                                              std::end(types),
		                                              std::begin(other.types),
		                                              std::end(other.types),
		                                              [](const auto& lhs, const auto& rhs) { return lhs <=> rhs; });
	}

	StructType::operator std::string() const
	{
		std::stringstream ss;
		ss << '(';
		for (size_t index = 0; index < types.size(); ++index) {
			ss << std::string(types[index]);
			if (index + 1 != types.size()) {
				ss << ", ";
			}
		}
		ss << ')';
		return ss.str();
	}

	std::ostream& operator<<(std::ostream& os, const StructType& type) { return os << std::string(type); }

}  // namespace Soul::Types
