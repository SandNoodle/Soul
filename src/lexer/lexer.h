#pragma once

#include "common/source_location.h"
#include "core/types.h"
#include "lexer/codepoint.h"
#include "lexer/token.h"

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
		[[nodiscard]] static std::vector<Token> tokenize(std::string_view script);

		private:
		Lexer(std::string_view);

		std::vector<Token> tokenize();

		std::string_view current_token(std::size_t exclude_start = 0, std::size_t exclude_end = 0);
		Token create_token(Token::Type type, std::string_view data);
		Token scan_token();

		CodePoint::ValueType peek_at(std::size_t n) const;
		CodePoint::ValueType advance();

		template <std::predicate<CodePoint::ValueType> Predicate>
		constexpr void advance_while(Predicate&& predicate) noexcept
		{
			auto current_codepoint = peek_at(0);
			while (current_codepoint != CodePoint::k_eof && predicate(current_codepoint)) {
				current_codepoint = advance();
			}
		}

		template <std::predicate<CodePoint::ValueType> Predicate>
		constexpr bool advance_if(Predicate&& predicate) noexcept
		{
			if (predicate(peek_at(0))) {
				advance_while(predicate);
				return true;
			}
			return false;
		}
	};
}  // namespace soul::lexer
