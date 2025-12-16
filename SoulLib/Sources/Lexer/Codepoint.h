#pragma once

#include "Soul.h"

namespace Soul
{
	/**
	 * @brief Represents a single character encoded in a UTF-8 format.
	 */
	struct Codepoint
	{
		public:
		using ValueType                  = UInt32;
		static constexpr ValueType k_eof = 0xFFFFFFFF;  // U+FFFF END OF FILE (EOF)

		public:
		Codepoint()                                = delete;
		Codepoint(const Codepoint&)                = delete;
		Codepoint(Codepoint&&) noexcept            = delete;
		~Codepoint()                               = delete;
		Codepoint& operator=(const Codepoint&)     = delete;
		Codepoint& operator=(Codepoint&&) noexcept = delete;

		SOUL_FORCE_INLINE static constexpr bool IsNewline(const ValueType c) noexcept
		{
			static constexpr ValueType k_end_of_line     = 0x0A;  // U+000A END OF LINE (\n)
			static constexpr ValueType k_form_feed       = 0x0C;  // U+000C FORM FEED (\f)
			static constexpr ValueType k_carriage_return = 0x0D;  // U+000D CARRIAGE RETURN (\r)
			return c == k_end_of_line || c == k_form_feed || c == k_carriage_return;
		}

		SOUL_FORCE_INLINE static constexpr bool IsWhitespace(const ValueType c) noexcept
		{
			static constexpr ValueType k_tabulation = 0x09;  // U+0009 CHARACTER TABULATION (\t)
			static constexpr ValueType k_space      = 0x20;  // U+0020 SPACE ( )
			return IsNewline(c) || c == k_tabulation || c == k_space;
		}

		SOUL_FORCE_INLINE static constexpr bool IsAsciiAlphabetic(const ValueType c) noexcept
		{
			static constexpr ValueType k_small_a   = 0x61;  // U+0061 Latin Small Letter A (a)
			static constexpr ValueType k_small_z   = 0x7A;  // U+007A Latin Small Letter Z (z)
			static constexpr ValueType k_capital_a = 0x41;  // U+0041 Latin Capital Letter A (A)
			static constexpr ValueType k_capital_z = 0x5A;  // U+005A Latin Capital Letter Z (Z)
			return (c >= k_small_a && c <= k_small_z) || (c >= k_capital_a && c <= k_capital_z);
		}

		SOUL_FORCE_INLINE static constexpr bool IsSign(const ValueType c) noexcept
		{
			static constexpr ValueType k_plus_sign  = 0x2B;  // U+002B PLUS SIGN (+)
			static constexpr ValueType k_minus_sign = 0x2D;  // U+002D HYPHEN (-)
			return c == k_plus_sign || c == k_minus_sign;
		}

		SOUL_FORCE_INLINE static constexpr bool IsDigit(const ValueType c) noexcept
		{
			static constexpr ValueType k_digit_zero = 0x30;  // U+0030 Digit Zero (0)
			static constexpr ValueType k_digit_nine = 0x39;  // U+0039 Digit Nine (9)
			return c >= k_digit_zero && c <= k_digit_nine;
		}

		SOUL_FORCE_INLINE static constexpr bool IsValidNumericContinuator(const ValueType c) noexcept
		{
			static constexpr ValueType k_full_stop = 0x2E;  // U+002E FULL STOP  (.)
			return IsDigit(c) || c == k_full_stop;
		}

		SOUL_FORCE_INLINE static constexpr bool IsValidStringStarter(const ValueType c) noexcept
		{
			static constexpr ValueType k_quotation_mark = 0x22;  // U+0022 Quotation Mark (")
			static constexpr ValueType k_apostrophe     = 0x27;  // U+0022 Apostrophe (')
			return c == k_quotation_mark || c == k_apostrophe;
		}

		SOUL_FORCE_INLINE static constexpr bool IsValidIdentifierStarter(const ValueType c) noexcept
		{
			static constexpr ValueType k_low_line = 0x5F;  // U+005F LOW LINE (_)
			return IsAsciiAlphabetic(c) || c == k_low_line;
		}

		SOUL_FORCE_INLINE static constexpr bool IsValidIdentifierContinuator(const ValueType c)
		{
			return IsValidIdentifierStarter(c) || IsDigit(c);
		}
	};
}  // namespace Soul
