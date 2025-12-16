#include "Lexer/Lexer.h"

#include <algorithm>
#include <array>
#include <functional>

namespace Soul::Lexer
{
#define MATCH_ONE_CODEPOINT(codepoint, token_type)                 \
	case codepoint:                                                \
	{                                                              \
		Advance(); /* Skip over the character */                   \
		return CreateToken(TokenType::token_type, CurrentToken()); \
	}

#define MATCH_TWO_CODEPOINTS(first_codepoint, second_codepoint, one_match_type, two_match_type) \
	case first_codepoint:                                                                       \
	{                                                                                           \
		Advance(); /* Skip over the first character */                                          \
		if (Peek() == (second_codepoint)) {                                                     \
			Advance(); /* Skip over the second character */                                     \
			return CreateToken(TokenType::two_match_type, CurrentToken());                      \
		}                                                                                       \
		return CreateToken(TokenType::one_match_type, CurrentToken());                          \
	}

#define MATCH_TWO_CODEPOINTS_SPECIAL(first_codepoint,                              \
                                     second_codepoint_a,                           \
                                     second_codepoint_b,                           \
                                     token_type_match_one,                         \
                                     token_type_match_two_a,                       \
                                     token_type_match_two_b)                       \
	case first_codepoint:                                                          \
	{                                                                              \
		Advance(); /* Skip over the first character */                             \
		Codepoint::ValueType next_codepoint = Peek();                              \
		if (next_codepoint == (second_codepoint_a)) {                              \
			Advance(); /* Skip over the second character */                        \
			return CreateToken(TokenType::token_type_match_two_a, CurrentToken()); \
		}                                                                          \
		if (next_codepoint == (second_codepoint_b)) {                              \
			Advance(); /* Skip over the second character */                        \
			return CreateToken(TokenType::token_type_match_two_b, CurrentToken()); \
		}                                                                          \
		return CreateToken(TokenType::token_type_match_one, CurrentToken());       \
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
			if (token.type == TokenType::TOKEN_SPECIAL_END_OF_FILE) {
				break;
			}
			result.push_back(std::move(token));
		}
		return result;
	}

	std::string_view Lexer::CurrentToken(std::size_t exclude_start, std::size_t exclude_end) const
	{
		return _script.substr(_offset_start + exclude_start,
		                      _offset_current - _offset_start - exclude_start - exclude_end);
	}

	Token Lexer::CreateToken(TokenType type, std::string_view data) const
	{
		SourceOffset location = SourceOffset{ _current_location.row,
			                                      std::min(_current_location.column,
			                                               _current_location.column - static_cast<u32>(data.size())) };
		return Token{
			.location = std::move(location),
			.type     = type,
		};
	};

	Token Lexer::ScanToken()
	{
		// Consume whitespace and comments.
		{
			bool keep_consuming = false;
			do {
				keep_consuming = false;
				AdvanceWhile(Codepoint::IsWhitespace);

				// Skip comments.
				if (Peek() == '#') {
					AdvanceWhile([](const Codepoint::ValueType c) -> bool { return !Codepoint::IsNewline(c); });
					keep_consuming = true;
				}
			} while (Peek() != Codepoint::k_eof && keep_consuming);
		};

		_offset_start = _offset_current;

		// --- End of File ---
		Codepoint::ValueType current_codepoint = Peek();
		if (current_codepoint == Codepoint::k_eof) {
			return CreateToken(TokenType::TOKEN_SPECIAL_END_OF_FILE, CurrentToken());
		}

		// Numeric
		if (Codepoint::IsDigit(Peek(Codepoint::IsSign(current_codepoint) ? 1 : 0))) {
			if (Codepoint::IsSign(current_codepoint)) {
				Advance();
			}
			AdvanceWhile(Codepoint::IsValidNumericContinuator);

			// NOTE: Assume that lexeme containing a dot (.) is a floating-point numeric literal candidate.
			TokenType numeric_type          = TokenType::TOKEN_LITERAL_INTEGER;
			std::string_view current_lexeme = CurrentToken();
			if (current_lexeme.contains('.')) {
				numeric_type = TokenType::TOKEN_LITERAL_FLOAT;
			}
			return CreateToken(numeric_type, current_lexeme);
		}

		// --- Strings ---
		if (Codepoint::IsValidStringStarter(current_codepoint)) {
			Advance();  // Skip '"'
			AdvanceWhile([](const Codepoint::ValueType c) -> bool { return c != '"'; });
			if (Peek() == Codepoint::k_eof) {
				static constexpr auto k_error_message = "unterminated string literal; did you forget '\"'?"sv;
				return CreateToken(TokenType::TOKEN_SPECIAL_ERROR, k_error_message);
			}
			Advance();  // Skip '"'
			return CreateToken(TokenType::TOKEN_LITERAL_STRING, CurrentToken(1, 1));
		}

		// --- Keywords & Literals ---
		if (Codepoint::IsValidIdentifierStarter(current_codepoint)) {
			AdvanceWhile(Codepoint::IsValidIdentifierContinuator);

			static constexpr std::array k_keywords = {
				// Keywords
				std::make_pair("break"sv, TokenType::TOKEN_KEYWORD_BREAK),
				std::make_pair("cast"sv, TokenType::TOKEN_KEYWORD_CAST),
				std::make_pair("continue"sv, TokenType::TOKEN_KEYWORD_CONTINUE),
				std::make_pair("else"sv, TokenType::TOKEN_KEYWORD_ELSE),
				std::make_pair("false"sv, TokenType::TOKEN_KEYWORD_FALSE),
				std::make_pair("fn"sv, TokenType::TOKEN_KEYWORD_FN),
				std::make_pair("for"sv, TokenType::TOKEN_KEYWORD_FOR),
				std::make_pair("if"sv, TokenType::TOKEN_KEYWORD_IF),
				std::make_pair("let"sv, TokenType::TOKEN_KEYWORD_LET),
				std::make_pair("mut"sv, TokenType::TOKEN_KEYWORD_MUT),
				std::make_pair("native"sv, TokenType::TOKEN_KEYWORD_NATIVE),
				std::make_pair("null"sv, TokenType::TOKEN_KEYWORD_NULL),
				std::make_pair("return"sv, TokenType::TOKEN_KEYWORD_RETURN),
				std::make_pair("struct"sv, TokenType::TOKEN_KEYWORD_STRUCT),
				std::make_pair("true"sv, TokenType::TOKEN_KEYWORD_TRUE),
				std::make_pair("while"sv, TokenType::TOKEN_KEYWORD_WHILE),
			};
			std::string_view lexeme = CurrentToken();
			const auto it           = std::ranges::find(k_keywords, lexeme, &decltype(k_keywords)::value_type::first);
			return CreateToken(it != std::end(k_keywords) ? it->second : TokenType::TOKEN_LITERAL_IDENTIFIER, lexeme);
		}

		// --- Symbols ---
		switch (current_codepoint) {
			// Simple cases, where there are no other characters in the sequence.
			MATCH_ONE_CODEPOINT('(', TOKEN_SYMBOL_PAREN_LEFT)
			MATCH_ONE_CODEPOINT(')', TOKEN_SYMBOL_PAREN_RIGHT)
			MATCH_ONE_CODEPOINT(',', TOKEN_SYMBOL_COMMA)
			MATCH_ONE_CODEPOINT('.', TOKEN_SYMBOL_DOT)
			MATCH_ONE_CODEPOINT(';', TOKEN_SYMBOL_SEMICOLON)
			MATCH_ONE_CODEPOINT('?', TOKEN_SYMBOL_QUESTION_MARK)
			MATCH_ONE_CODEPOINT('[', TOKEN_SYMBOL_BRACKET_LEFT)
			MATCH_ONE_CODEPOINT(']', TOKEN_SYMBOL_BRACKET_RIGHT)
			MATCH_ONE_CODEPOINT('^', TOKEN_SYMBOL_CARET)
			MATCH_ONE_CODEPOINT('{', TOKEN_SYMBOL_BRACE_LEFT)
			MATCH_ONE_CODEPOINT('}', TOKEN_SYMBOL_BRACE_RIGHT)

			// Tokens which might have one other character in the sequence.
			MATCH_TWO_CODEPOINTS('%', '=', TOKEN_SYMBOL_PERCENT, TOKEN_SYMBOL_PERCENT_EQUAL)
			MATCH_TWO_CODEPOINTS('!', '=', TOKEN_SYMBOL_BANG, TOKEN_SYMBOL_BANG_EQUAL)
			MATCH_TWO_CODEPOINTS('&', '&', TOKEN_SYMBOL_AMPERSAND, TOKEN_SYMBOL_AMPERSAND_AMPERSAND)
			MATCH_TWO_CODEPOINTS('*', '=', TOKEN_SYMBOL_STAR, TOKEN_SYMBOL_STAR_EQUAL)
			MATCH_TWO_CODEPOINTS('/', '=', TOKEN_SYMBOL_SLASH, TOKEN_SYMBOL_SLASH_EQUAL)
			MATCH_TWO_CODEPOINTS(':', ':', TOKEN_SYMBOL_COLON, TOKEN_SYMBOL_COLON_COLON)
			MATCH_TWO_CODEPOINTS('<', '=', TOKEN_SYMBOL_LESS, TOKEN_SYMBOL_LESS_EQUAL)
			MATCH_TWO_CODEPOINTS('=', '=', TOKEN_SYMBOL_EQUAL, TOKEN_SYMBOL_EQUAL_EQUAL)
			MATCH_TWO_CODEPOINTS('>', '=', TOKEN_SYMBOL_GREATER, TOKEN_SYMBOL_GREATER_EQUAL)
			MATCH_TWO_CODEPOINTS('|', '|', TOKEN_SYMBOL_PIPE, TOKEN_SYMBOL_PIPE_PIPE)

			// Special cases, where the second character might be either present or vary.
			MATCH_TWO_CODEPOINTS_SPECIAL(
				'+', '+', '=', TOKEN_SYMBOL_PLUS, TOKEN_SYMBOL_PLUS_PLUS, TOKEN_SYMBOL_PLUS_EQUAL)
			MATCH_TWO_CODEPOINTS_SPECIAL(
				'-', '-', '=', TOKEN_SYMBOL_MINUS, TOKEN_SYMBOL_MINUS_MINUS, TOKEN_SYMBOL_MINUS_EQUAL)
			default:
				break;
		}

		static constexpr auto k_error_message = "unrecognized token"sv;
		return CreateToken(TokenType::TOKEN_SPECIAL_ERROR, k_error_message);
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
		if (Codepoint::IsNewline(_script[_offset_current++])) {
			++_current_location.row;
			_current_location.column = 0;
		} else {
			++_current_location.column;
		}
		return Peek();
	}
};  // namespace Soul::Lexer
