#include "common/types/type.h"

#include <sstream>
#include <unordered_map>

namespace soul::types
{
	Type::operator std::string() const
	{
		return std::visit([](const auto& arg) -> std::string { return std::string(arg); }, _type);
	}

	std::ostream& operator<<(std::ostream& os, const Type& type) { return os << std::string(type); }

	PrimitiveType::operator std::string() const
	{
		using namespace std::string_view_literals;
		static constexpr auto k_unknown                                          = "__unknown__"sv;
		static std::unordered_map<PrimitiveType::Kind, std::string_view> k_types = {
			{ PrimitiveType::Kind::Unknown, k_unknown   },
            { PrimitiveType::Kind::Boolean, "bool"sv    },
			{ PrimitiveType::Kind::Char,    "char"sv    },
            { PrimitiveType::Kind::Float32, "float32"sv },
			{ PrimitiveType::Kind::Float64, "float64"sv },
            { PrimitiveType::Kind::Int32,   "int32"sv   },
			{ PrimitiveType::Kind::Int64,   "int64"sv   },
            { PrimitiveType::Kind::String,  "string"sv  },
			{ PrimitiveType::Kind::Void,    "void"sv    },
		};
		if (!k_types.contains(type)) [[unlikely]] {
			return std::string(k_unknown);
		}
		return std::string(k_types.at(type));
	}

	std::ostream& operator<<(std::ostream& os, const PrimitiveType& type) { return os << std::string(type); }

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

	const Type& ArrayType::data_type() const noexcept { return *_type; }

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

}  // namespace soul::types
