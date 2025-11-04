#include "common/value.h"

#include <algorithm>
#include <sstream>

namespace soul
{
	template <>
	Scalar::operator std::string() const
	{
		return std::visit(
			[](const auto& v) -> std::string {
				if constexpr (std::is_constructible_v<std::decay_t<decltype(v)>, std::string>) {
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
	types::Type Scalar::type() const
	{
		return std::visit([](const auto& v) -> types::Type { return v.type(); }, _value);
	}

	types::Type Identifier::type() const noexcept
	{
		// NOTE: We can't know the Identifier's type before resolving, thus unknown.
		return types::Type{};
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

	types::Type Array::type() const
	{
		if (_values.empty()) [[unlikely]] {
			return types::Type{};
		}

		const auto& first_type = _values[0].type();
		const bool is_same_type
			= std::ranges::any_of(_values, [&first_type](const auto& v) -> bool { return v.type() != first_type; });
		if (!is_same_type) [[unlikely]] {
			// NOTE: We expect all array values to have the same type.
			return types::Type{ types::ArrayType{ types::Type{} } };
		}

		return types::Type{ types::ArrayType{ first_type } };
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

	types::Type Struct::type() const
	{
		types::StructType::ContainedTypes member_types{};
		member_types.reserve(_members.size());
		for (const auto& member : _members) {
			member_types.emplace_back(member.type());
		}
		return types::Type{ types::StructType{ std::move(member_types) } };
	}

	Value::operator std::string() const
	{
		return std::visit(
			[](const auto& v) -> std::string {
				if constexpr (!std::same_as<std::remove_cvref_t<decltype(v)>, UnknownValue>) {
					return std::string(v);
				} else {
					return std::string("__unknown__");
				}
			},
			_value);
	}

	std::string_view Value::internal_name(const Value& value)
	{
		using namespace std::string_view_literals;
		static constexpr auto k_unknown = "__unknown__"sv;
		return std::visit(
			[&value](const auto& v) -> std::string_view {
				if constexpr (std::same_as<std::remove_cvref_t<decltype(v)>, Scalar>) {
					const auto& scalar = value.as<Scalar>();
					if (scalar.is<types::PrimitiveType::Kind::Boolean>()) {
						return "value_type_boolean"sv;
					}
					if (scalar.is<types::PrimitiveType::Kind::Char>()) {
						return "value_type_char"sv;
					}
					if (scalar.is<types::PrimitiveType::Kind::Float32>()) {
						return "value_type_float32"sv;
					}
					if (scalar.is<types::PrimitiveType::Kind::Float64>()) {
						return "value_type_float64"sv;
					}
					if (scalar.is<types::PrimitiveType::Kind::Int32>()) {
						return "value_type_int32"sv;
					}
					if (scalar.is<types::PrimitiveType::Kind::Int64>()) {
						return "value_type_int64"sv;
					}
					if (scalar.is<types::PrimitiveType::Kind::String>()) {
						return "value_type_string"sv;
					}
					return k_unknown;
				} else if constexpr (std::same_as<std::remove_cvref_t<decltype(v)>, Identifier>) {
					return "value_type_identifier"sv;
				} else if constexpr (std::same_as<std::remove_cvref_t<decltype(v)>, Array>) {
					return "value_type_array"sv;
				} else if constexpr (std::same_as<std::remove_cvref_t<decltype(v)>, Struct>) {
					return "value_type_struct"sv;
				} else {
					return k_unknown;
				}
			},
			value._value);
	}

	types::Type Value::type() const
	{
		return std::visit(
			[](const auto& v) -> types::Type {
				if constexpr (!std::same_as<std::remove_cvref_t<decltype(v)>, UnknownValue>) {
					return v.type();
				} else {
					return types::Type{};
				}
			},
			_value);
	}

	std::partial_ordering operator<=>(const Value& lhs, const Value& rhs) { return lhs._value <=> rhs._value; }

}  // namespace soul
