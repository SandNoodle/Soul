#include "Parser/TypeSpecifier.h"

#include <format>

namespace Soul::Parser
{
	using namespace std::string_view_literals;

	BaseTypeSpecifier::BaseTypeSpecifier(ViewType value) : _value(std::string(value)) {}

	BaseTypeSpecifier::operator std::string() const { return _value; }

	PointerTypeSpecifier::PointerTypeSpecifier(TypeSpecifier&& value)
		: _value(std::make_unique<TypeSpecifier>(std::move(value)))
	{
	}

	PointerTypeSpecifier::PointerTypeSpecifier(const PointerTypeSpecifier& other) noexcept
		: _value(std::make_unique<TypeSpecifier>(*other._value.get()))
	{
	}

	PointerTypeSpecifier::PointerTypeSpecifier(PointerTypeSpecifier&& other) noexcept
		: _value(std::make_unique<TypeSpecifier>(*other._value.get()))
	{
	}

	PointerTypeSpecifier& PointerTypeSpecifier::operator=(const PointerTypeSpecifier& other) noexcept
	{
		if (this == &other) {
			return *this;
		}
		_value = std::make_unique<TypeSpecifier>(*other._value.get());
		return *this;
	}

	PointerTypeSpecifier& PointerTypeSpecifier::operator=(PointerTypeSpecifier&& other) noexcept
	{
		if (this == &other) {
			return *this;
		}
		*_value = std::move(*other._value.get());
		return *this;
	}

	std::strong_ordering PointerTypeSpecifier::operator<=>(const PointerTypeSpecifier& other) const
	{
		return *_value <=> *other._value;
	}

	PointerTypeSpecifier::operator std::string() const { return "*" + std::string(*_value); }

	ArrayTypeSpecifier::ArrayTypeSpecifier(TypeSpecifier&& value, Size size)
		: _value(std::make_unique<TypeSpecifier>(std::move(value))), _size(size)
	{
	}

	ArrayTypeSpecifier::ArrayTypeSpecifier(const ArrayTypeSpecifier& other) noexcept
		: _value(std::make_unique<TypeSpecifier>(*other._value.get())), _size(other._size)
	{
	}

	ArrayTypeSpecifier::ArrayTypeSpecifier(ArrayTypeSpecifier&& other) noexcept
		: _value(std::make_unique<TypeSpecifier>(*other._value.get())), _size(other._size)
	{
	}

	ArrayTypeSpecifier& ArrayTypeSpecifier::operator=(const ArrayTypeSpecifier& other) noexcept
	{
		if (this == &other) {
			return *this;
		}
		_value = std::make_unique<TypeSpecifier>(*other._value.get());
		_size  = other._size;
		return *this;
	}

	ArrayTypeSpecifier& ArrayTypeSpecifier::operator=(ArrayTypeSpecifier&& other) noexcept
	{
		if (this == &other) {
			return *this;
		}
		*_value = std::move(*other._value.get());
		_size   = other._size;
		return *this;
	}

	std::strong_ordering ArrayTypeSpecifier::operator<=>(const ArrayTypeSpecifier& other) const
	{
		return *_value <=> *other._value;
	}

	ArrayTypeSpecifier::operator std::string() const
	{
		if (_size == k_unbound_size) {
			return std::format("[{}]", std::string(*_value));
		}
		return std::format("[{}, {}]", std::string(*_value), _size);
	}

	TypeSpecifier::operator std::string() const
	{
		return std::visit(
			[](const auto& v) -> std::string {
				if constexpr (!std::same_as<std::remove_cvref_t<decltype(v)>, std::monostate>) {
					return std::string(v);
				} else {
					return std::string("__unknown__");
				}
			},
			_value);
	}

	const BaseTypeSpecifier k_base_specifier_i8    = BaseTypeSpecifier{ "i8"sv };
	const BaseTypeSpecifier k_base_specifier_i16   = BaseTypeSpecifier{ "i16"sv };
	const BaseTypeSpecifier k_base_specifier_i32   = BaseTypeSpecifier{ "i32"sv };
	const BaseTypeSpecifier k_base_specifier_i64   = BaseTypeSpecifier{ "i64"sv };
	const BaseTypeSpecifier k_base_specifier_i128  = BaseTypeSpecifier{ "i128"sv };
	const BaseTypeSpecifier k_base_specifier_u8    = BaseTypeSpecifier{ "u8"sv };
	const BaseTypeSpecifier k_base_specifier_u16   = BaseTypeSpecifier{ "u16"sv };
	const BaseTypeSpecifier k_base_specifier_u32   = BaseTypeSpecifier{ "u32"sv };
	const BaseTypeSpecifier k_base_specifier_u64   = BaseTypeSpecifier{ "u64"sv };
	const BaseTypeSpecifier k_base_specifier_u128  = BaseTypeSpecifier{ "u128"sv };
	const BaseTypeSpecifier k_base_specifier_f32   = BaseTypeSpecifier{ "f32"sv };
	const BaseTypeSpecifier k_base_specifier_f64   = BaseTypeSpecifier{ "f64"sv };
	const BaseTypeSpecifier k_base_specifier_chr   = BaseTypeSpecifier{ "chr"sv };
	const BaseTypeSpecifier k_base_specifier_str   = BaseTypeSpecifier{ "str"sv };
	const BaseTypeSpecifier k_base_specifier_bool  = BaseTypeSpecifier{ "bool"sv };
	const BaseTypeSpecifier k_base_specifier_isize = BaseTypeSpecifier{ "isize"sv };
	const BaseTypeSpecifier k_base_specifier_usize = BaseTypeSpecifier{ "usize"sv };
	const BaseTypeSpecifier k_base_specifier_void  = BaseTypeSpecifier{ "void"sv };
}  // namespace Soul::Parser
