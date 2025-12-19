#include "Common/Value.h"

#include <algorithm>
#include <sstream>

namespace Soul
{
	template <>
	Scalar::operator std::string() const
	{
		return std::visit(
			[]<typename T>(const T& v) -> std::string {
				if constexpr (std::is_constructible_v<std::decay_t<T>, std::string>) {
					return std::string(v);
				} else {
					std::stringstream ss;
					ss << std::boolalpha << v;
					return ss.str();
				}
			},
			_value);
	}

	template <>
	Types::Type Scalar::GetType() const
	{
		return std::visit([](const auto& v) -> Types::Type { return v.GetType(); }, _value);
	}

	Types::Type Identifier::GetType() const noexcept
	{
		// NOTE: We can't know the Identifier's type before resolving, thus unknown.
		return Types::Type{};
	}

	Identifier::operator std::string() const { return _value; }

	Array::operator std::string() const
	{
		std::stringstream ss;
		ss << '[';
		for (std::size_t index = 0; index < _values.size(); ++index) {
			ss << std::string(_values[index]);
			if (index != _values.size() - 1) {
				ss << ", ";
			}
		}
		ss << ']';
		return ss.str();
	}

	Types::Type Array::GetType() const
	{
		if (_values.empty()) [[unlikely]] {
			return Types::Type{};
		}

		const auto& first_type = _values[0].GetType();
		const bool is_same_type
			= std::ranges::any_of(_values, [&first_type](const auto& v) -> bool { return v.GetType() != first_type; });
		if (!is_same_type) [[unlikely]] {
			// NOTE: We expect all array values to have the same type.
			return Types::Type{ Types::ArrayType{ Types::Type{} } };
		}

		return Types::Type{ Types::ArrayType{ first_type } };
	}

	Struct::operator std::string() const
	{
		std::stringstream ss;
		ss << '(';
		for (std::size_t index = 0; index < _members.size(); ++index) {
			ss << std::string(_members[index]);
			if (index != _members.size() - 1) {
				ss << ", ";
			}
		}
		ss << ')';
		return ss.str();
	}

	Types::Type Struct::GetType() const
	{
		Types::StructType::ContainedTypes member_types{};
		member_types.reserve(_members.size());
		for (const auto& member : _members) {
			member_types.emplace_back(member.GetType());
		}
		return Types::Type{ Types::StructType{ std::move(member_types) } };
	}

	Value::operator std::string() const
	{
		return std::visit(
			[]<typename T>(const T& v) -> std::string {
				if constexpr (!std::same_as<std::remove_cvref_t<T>, UnknownValue>) {
					return std::string(v);
				} else {
					return std::string("__unknown__");
				}
			},
			_value);
	}

	std::string_view Value::NameInternal(const Value& value)
	{
		using namespace std::string_view_literals;
		static constexpr auto k_unknown = "__unknown__"sv;
		return std::visit(
			[&value]<typename T>(const T& v) -> std::string_view {
				if constexpr (std::same_as<std::remove_cvref_t<T>, Scalar>) {
					const auto& scalar = value.As<Scalar>();
					if (scalar.Is<Types::PrimitiveType::Kind::BOOLEAN>()) {
						return "value_type_boolean"sv;
					}
					if (scalar.Is<Types::PrimitiveType::Kind::CHAR>()) {
						return "value_type_char"sv;
					}
					if (scalar.Is<Types::PrimitiveType::Kind::FLOAT32>()) {
						return "value_type_float32"sv;
					}
					if (scalar.Is<Types::PrimitiveType::Kind::FLOAT64>()) {
						return "value_type_float64"sv;
					}
					if (scalar.Is<Types::PrimitiveType::Kind::INT32>()) {
						return "value_type_int32"sv;
					}
					if (scalar.Is<Types::PrimitiveType::Kind::INT64>()) {
						return "value_type_int64"sv;
					}
					if (scalar.Is<Types::PrimitiveType::Kind::STRING>()) {
						return "value_type_string"sv;
					}
					return k_unknown;
				} else if constexpr (std::same_as<std::remove_cvref_t<T>, Identifier>) {
					return "value_type_identifier"sv;
				} else if constexpr (std::same_as<std::remove_cvref_t<T>, Array>) {
					return "value_type_array"sv;
				} else if constexpr (std::same_as<std::remove_cvref_t<T>, Struct>) {
					return "value_type_struct"sv;
				} else {
					return k_unknown;
				}
			},
			value._value);
	}

	Types::Type Value::GetType() const
	{
		return std::visit(
			[]<typename T>(const T& v) -> Types::Type {
				if constexpr (!std::same_as<std::remove_cvref_t<T>, UnknownValue>) {
					return v.GetType();
				} else {
					return Types::Type{};
				}
			},
			_value);
	}

	std::partial_ordering operator<=>(const Value& lhs, const Value& rhs) { return lhs._value <=> rhs._value; }

}  // namespace Soul
