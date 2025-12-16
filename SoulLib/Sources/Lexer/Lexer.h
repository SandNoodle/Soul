#pragma once

#include "Core/Diagnostic.h"
#include "Core/StringView.h"
#include "Lexer/Codepoint.h"
#include "Lexer/Token.h"

#include <vector>

namespace Soul::Lexer
{
	/**
	 * @brief Lexer performs lexical analysis on an input text, i.e. converts it into a linear sequence of lexical
	 * tokens (called Lexemes).
	 */
	class Lexer
	{
		private:
		std::vector<Diagnostic> _diagnostics;
		StringView _script;
		UInt32 _offset_begin;
		UInt32 _offset_end;

		public:
		[[nodiscard]] static std::vector<Token> Tokenize(StringView script);

		private:
		explicit Lexer(StringView);

		std::vector<Token> Tokenize();

		Token ScanToken();
		Token CreateToken(TokenType type) const;
		Token CreateError(Diagnostic diagnostic);
		StringView CurrentLexeme();

		Codepoint::ValueType Peek(std::size_t n = 0) const;
		Codepoint::ValueType Advance();

		template <std::predicate<Codepoint::ValueType> Predicate>
		constexpr void AdvanceWhile(Predicate&& predicate) noexcept
		{
			auto current_codepoint = Peek();
			while (current_codepoint != Codepoint::k_eof && predicate(current_codepoint)) {
				current_codepoint = Advance();
			}
		}
	};
}  // namespace Soul::Lexer
