#pragma once

#include "common/source_location.h"
#include "core/types.h"
#include "lexer/Codepoint.h"
#include "lexer/Token.h"

#include <string_view>
#include <vector>

namespace soul::lexer
{
	/**
	 * @brief Lexer performs lexical analysis on an input text, i.e. converts it into a linear sequence of lexical
	 * tokens (called Lexemes).
	 */
	class Lexer
	{
		private:
		std::string_view _script{};
		std::size_t _offset_start;
		std::size_t _offset_current;
		SourceLocation _current_location;

		public:
		[[nodiscard]] static std::vector<Token> Tokenize(std::string_view script);

		private:
		explicit Lexer(std::string_view);

		std::vector<Token> Tokenize();

		std::string_view CurrentLexeme(std::size_t exclude_start = 0, std::size_t exclude_end = 0);
		Token CreateToken(Token::Type type, std::string_view data);
		Token ScanToken();

		Codepoint::ValueType Peek(std::size_t n) const;
		Codepoint::ValueType Advance();

		template <std::predicate<Codepoint::ValueType> Predicate>
		constexpr void AdvanceWhile(Predicate&& predicate) noexcept
		{
			auto current_codepoint = Peek(0);
			while (current_codepoint != Codepoint::k_eof && predicate(current_codepoint)) {
				current_codepoint = Advance();
			}
		}

		template <std::predicate<Codepoint::ValueType> Predicate>
		constexpr bool AdvanceIf(Predicate&& predicate) noexcept
		{
			if (predicate(Peek(0))) {
				AdvanceWhile(predicate);
				return true;
			}
			return false;
		}
	};
}  // namespace soul::lexer
