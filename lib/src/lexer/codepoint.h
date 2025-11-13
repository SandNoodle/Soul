#pragma once

#include "core/types.h"

namespace soul
{
	/**
	 * @brief Represents a single character encoded in a UTF-8 format.
	 */
	struct CodePoint
	{
		public:
		using ValueType                              = u32;
		static constexpr ValueType k_eof             = 0xFFFFFFFF;  // U+FFFF END OF FILE (EOF)
		static constexpr ValueType k_tabulation      = 0x09;        // U+0009 CHARACTER TABULATION (\t)
		static constexpr ValueType k_end_of_line     = 0x0A;        // U+000A END OF LINE (\n)
		static constexpr ValueType k_form_feed       = 0x0C;        // U+000C FORM FEED (\f)
		static constexpr ValueType k_carriage_return = 0x0D;        // U+000D CARRIAGE RETURN (\r)
		static constexpr ValueType k_space           = 0x20;        // U+0020 SPACE ( )
		static constexpr ValueType k_plus_sign       = 0x2B;        // U+002B PLUS SIGN (+)
		static constexpr ValueType k_hyphen          = 0x2D;        // U+002D HYPHEN (-)
		static constexpr ValueType k_full_stop       = 0x2E;        // U+002E FULL STOP  (.)
		static constexpr ValueType k_low_line        = 0x5F;        // U+005F LOW LINE (_)

		public:
		CodePoint()                                = delete;
		CodePoint(const CodePoint&)                = delete;
		CodePoint(CodePoint&&) noexcept            = delete;
		~CodePoint()                               = delete;
		CodePoint& operator=(const CodePoint&)     = delete;
		CodePoint& operator=(CodePoint&&) noexcept = delete;

		static constexpr bool is_newline(CodePoint::ValueType c) noexcept
		{
			return c == k_end_of_line || c == k_form_feed || c == k_carriage_return;
		}

		static constexpr bool is_whitespace(CodePoint::ValueType c) noexcept
		{
			return is_newline(c) || c == k_tabulation || c == k_space;
		}

		static constexpr bool is_alpha(CodePoint::ValueType c) noexcept
		{
			return (c >= 0x61 && c <= 0x7A) || (c >= 0x41 && c <= 0x5A);
		}

		static constexpr bool is_digit(CodePoint::ValueType c) noexcept { return c >= 0x30 && c <= 0x39; }

		static constexpr bool is_identifier(CodePoint::ValueType c) noexcept
		{
			return is_alpha(c) || c == CodePoint::k_low_line;
		}
	};
}  // namespace soul
