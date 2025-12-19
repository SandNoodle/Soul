#include "Lexer/Lexer.h"

#include <algorithm>
#include <array>
#include <functional>

namespace Soul::Lexer
{
#define MATCH_ONE_CODEPOINT(codepoint, token_type)       \
	case codepoint:                                      \
	{                                                    \
		Advance(); /* Skip over the character */         \
		return CreateToken(token_type, CurrentLexeme()); \
	}

#define MATCH_TWO_CODEPOINTS(first_codepoint, second_codepoint, one_match_type, two_match_type) \
	case first_codepoint:                                                                       \
	{                                                                                           \
		Advance(); /* Skip over the first character */                                          \
		if (Peek(0) == (second_codepoint)) {                                                    \
			Advance(); /* Skip over the second character */                                     \
			return CreateToken(two_match_type, CurrentLexeme());                                \
		}                                                                                       \
		return CreateToken(one_match_type, CurrentLexeme());                                    \
	}

#define MATCH_TWO_CODEPOINTS_SPECIAL(first_codepoint,                    \
                                     second_codepoint_a,                 \
                                     second_codepoint_b,                 \
                                     token_type_match_one,               \
                                     token_type_match_two_a,             \
                                     token_type_match_two_b)             \
	case first_codepoint:                                                \
	{                                                                    \
		Advance(); /* Skip over the first character */                   \
		Codepoint::ValueType next_codepoint = Peek(0);                   \
		if (next_codepoint == (second_codepoint_a)) {                    \
			Advance(); /* Skip over the second character */              \
			return CreateToken(token_type_match_two_a, CurrentLexeme()); \
		}                                                                \
		if (next_codepoint == (second_codepoint_b)) {                    \
			Advance(); /* Skip over the second character */              \
			return CreateToken(token_type_match_two_b, CurrentLexeme()); \
		}                                                                \
		return CreateToken(token_type_match_one, CurrentLexeme());       \
	}

	using namespace std::string_view_literals;

	Lexer::Lexer(std::string_view script)
		: _script(script), _offset_start(0), _offset_current(0), _current_location(1, 0)
	{
	}

	std::vector<Token> Lexer::Tokenize(std::string_view script) { return Lexer{ script }.Tokenize(); }

	std::vector<Token> Lexer::Tokenize()
	{
		std::vector<Token> result;
		if (_script.empty()) {
			return result;
		}

		for (;;) {
			Token token = ScanToken();
			if (token.type == Token::Type::SPECIAL_END_OF_FILE) {
				break;
			}
			result.push_back(token);
		}
		return result;
	}

	std::string_view Lexer::CurrentLexeme(std::size_t exclude_start, std::size_t exclude_end) const
	{
		return _script.substr(_offset_start + exclude_start,
		                      _offset_current - _offset_start - exclude_start - exclude_end);
	}

	Token Lexer::CreateToken(Token::Type type, std::string_view data) const
	{
		SourceOffset location = SourceOffset{ _current_location.row,
			                                  std::min(_current_location.column,
			                                           _current_location.column - static_cast<UInt32>(data.size())) };
		return Token{ type, data, location };
	}

	Token Lexer::ScanToken()
	{
		// Consume whitespace and comments.
		{
			bool keep_consuming = false;
			do {
				keep_consuming = false;
				AdvanceWhile(Codepoint::IsWhitespace);

				// Skip comments.
				if (Peek(0) == '#') {
					AdvanceWhile(std::not_fn(Codepoint::IsNewline));
					keep_consuming = true;
				}
			} while (Peek(0) != Codepoint::k_eof && keep_consuming);
		}

		_offset_start = _offset_current;

		Codepoint::ValueType current_codepoint = Peek(0);
		if (current_codepoint == Codepoint::k_eof) {
			return CreateToken(Token::Type::SPECIAL_END_OF_FILE, CurrentLexeme());
		}

		// Numeric
		const bool has_sign = current_codepoint == Codepoint::k_hyphen || current_codepoint == Codepoint::k_plus_sign;
		if (Codepoint::IsDigit(Peek(has_sign ? 1 : 0))) {
			AdvanceWhile([](const auto c) -> bool {
				return Codepoint::IsDigit(c) || c == Codepoint::k_full_stop || c == Codepoint::k_plus_sign
				    || c == Codepoint::k_hyphen;
			});
			const auto lexeme = CurrentLexeme();
			const bool has_decimal_point
				= std::ranges::any_of(lexeme, [](const auto c) -> bool { return c == Codepoint::k_full_stop; });
			return CreateToken(has_decimal_point ? Token::Type::LITERAL_FLOAT : Token::Type::LITERAL_INTEGER, lexeme);
		}

		// Strings
		if (current_codepoint == '"') {
			Advance();  // Skip '"'
			AdvanceWhile([](const auto c) -> bool { return c != '"'; });
			if (Peek(0) == Codepoint::k_eof) {
				static constexpr auto k_error_message = "unterminated string literal; did you forget '\"'?"sv;
				return CreateToken(Token::Type::SPECIAL_ERROR, k_error_message);
			}
			Advance();  // Skip '"'
			return CreateToken(Token::Type::LITERAL_STRING, CurrentLexeme(1, 1));
		}

		// Keywords & Literals
		if (AdvanceIf([](const auto c) -> bool { return Codepoint::IsIdentifier(c) || Codepoint::IsDigit(c); })) {
			static constexpr std::array k_keywords = {
				// Keywords
				std::make_pair("break"sv, Token::Type::KEYWORD_BREAK),
				std::make_pair("cast"sv, Token::Type::KEYWORD_CAST),
				std::make_pair("continue"sv, Token::Type::KEYWORD_CONTINUE),
				std::make_pair("else"sv, Token::Type::KEYWORD_ELSE),
				std::make_pair("false"sv, Token::Type::KEYWORD_FALSE),
				std::make_pair("fn"sv, Token::Type::KEYWORD_FN),
				std::make_pair("for"sv, Token::Type::KEYWORD_FOR),
				std::make_pair("if"sv, Token::Type::KEYWORD_IF),
				std::make_pair("let"sv, Token::Type::KEYWORD_LET),
				std::make_pair("mut"sv, Token::Type::KEYWORD_MUT),
				std::make_pair("native"sv, Token::Type::KEYWORD_NATIVE),
				std::make_pair("null"sv, Token::Type::KEYWORD_NULL),
				std::make_pair("return"sv, Token::Type::KEYWORD_RETURN),
				std::make_pair("struct"sv, Token::Type::KEYWORD_STRUCT),
				std::make_pair("true"sv, Token::Type::KEYWORD_TRUE),
				std::make_pair("while"sv, Token::Type::KEYWORD_WHILE),
			};
			const auto lexeme = CurrentLexeme();
			const auto it     = std::ranges::find(k_keywords, lexeme, &decltype(k_keywords)::value_type::first);
			return CreateToken(it != std::end(k_keywords) ? it->second : Token::Type::LITERAL_IDENTIFIER, lexeme);
		}

		// Symbols
		switch (current_codepoint) {
			// Simple cases, where there are no other characters in the sequence.
			MATCH_ONE_CODEPOINT('(', Token::Type::SYMBOL_PAREN_LEFT)
			MATCH_ONE_CODEPOINT(')', Token::Type::SYMBOL_PAREN_RIGHT)
			MATCH_ONE_CODEPOINT(',', Token::Type::SYMBOL_COMMA)
			MATCH_ONE_CODEPOINT('.', Token::Type::SYMBOL_DOT)
			MATCH_ONE_CODEPOINT(';', Token::Type::SYMBOL_SEMICOLON)
			MATCH_ONE_CODEPOINT('?', Token::Type::SYMBOL_QUESTION_MARK)
			MATCH_ONE_CODEPOINT('[', Token::Type::SYMBOL_BRACKET_LEFT)
			MATCH_ONE_CODEPOINT(']', Token::Type::SYMBOL_BRACKET_RIGHT)
			MATCH_ONE_CODEPOINT('^', Token::Type::SYMBOL_CARET)
			MATCH_ONE_CODEPOINT('{', Token::Type::SYMBOL_BRACE_LEFT)
			MATCH_ONE_CODEPOINT('}', Token::Type::SYMBOL_BRACE_RIGHT)

			// Tokens which might have one other character in the sequence.
			MATCH_TWO_CODEPOINTS('%', '=', Token::Type::SYMBOL_PERCENT, Token::Type::SYMBOL_PERCENT_EQUAL)
			MATCH_TWO_CODEPOINTS('!', '=', Token::Type::SYMBOL_BANG, Token::Type::SYMBOL_BANG_EQUAL)
			MATCH_TWO_CODEPOINTS('&', '&', Token::Type::SYMBOL_AMPERSAND, Token::Type::SYMBOL_AMPERSAND_AMPERSAND)
			MATCH_TWO_CODEPOINTS('*', '=', Token::Type::SYMBOL_STAR, Token::Type::SYMBOL_STAR_EQUAL)
			MATCH_TWO_CODEPOINTS('/', '=', Token::Type::SYMBOL_SLASH, Token::Type::SYMBOL_SLASH_EQUAL)
			MATCH_TWO_CODEPOINTS(':', ':', Token::Type::SYMBOL_COLON, Token::Type::SYMBOL_COLON_COLON)
			MATCH_TWO_CODEPOINTS('<', '=', Token::Type::SYMBOL_LESS, Token::Type::SYMBOL_LESS_EQUAL)
			MATCH_TWO_CODEPOINTS('=', '=', Token::Type::SYMBOL_EQUAL, Token::Type::SYMBOL_EQUAL_EQUAL)
			MATCH_TWO_CODEPOINTS('>', '=', Token::Type::SYMBOL_GREATER, Token::Type::SYMBOL_GREATER_EQUAL)
			MATCH_TWO_CODEPOINTS('|', '|', Token::Type::SYMBOL_PIPE, Token::Type::SYMBOL_PIPE_PIPE)

			// Special cases, where the second character might be either present or vary.
			MATCH_TWO_CODEPOINTS_SPECIAL(
				'+', '+', '=', Token::Type::SYMBOL_PLUS, Token::Type::SYMBOL_PLUS_PLUS, Token::Type::SYMBOL_PLUS_EQUAL)
			MATCH_TWO_CODEPOINTS_SPECIAL('-',
			                             '-',
			                             '=',
			                             Token::Type::SYMBOL_MINUS,
			                             Token::Type::SYMBOL_MINUS_MINUS,
			                             Token::Type::SYMBOL_MINUS_EQUAL)
			default:
				break;
		}

		static constexpr auto k_error_message = "unrecognized token"sv;
		return CreateToken(Token::Type::SPECIAL_ERROR, k_error_message);
	}

	Codepoint::ValueType Lexer::Peek(std::size_t n) const
	{
		if (_offset_current + n >= _script.size()) {
			return Codepoint::k_eof;
		}
		return _script[_offset_current + n];
	}

	Codepoint::ValueType Lexer::Advance()
	{
		++_current_location.column;
		if (Codepoint::IsNewline(_script[_offset_current++])) {
			++_current_location.row;
			_current_location.column = 0;
		}
		return Peek(0);
	}
}  // namespace Soul::Lexer
