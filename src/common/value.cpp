#include "common/value.h"

#include <sstream>

namespace soul
{
	template <>
	Scalar::operator std::string() const
	{
		return std::visit(
			[](const auto& v) -> std::string {
				if constexpr (std::is_same_v<std::remove_cvref_t<decltype(v)>, std::monostate>) {
					return std::string("__unknown__");
				} else if constexpr (std::is_constructible_v<std::decay_t<decltype(v)>, std::string>) {
					return std::string(v);
				} else {
					std::stringstream ss;
					ss << std::boolalpha << v;
					return ss.str();
				}
			},
			_value);
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

	Value::Value(Scalar value) : _value(std::move(value)) {}

	Value::Value(Identifier value) : _value(std::move(value)) {}

	Value::Value(Variant value) : _value(std::move(value)) {}
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

	std::partial_ordering operator<=>(const Value& lhs, const Value& rhs) noexcept { return lhs._value <=> rhs._value; }

}  // namespace soul
