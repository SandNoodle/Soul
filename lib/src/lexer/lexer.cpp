#include "lexer/lexer.h"

#include <algorithm>
#include <array>
#include <functional>

namespace soul::lexer
{
#define MATCH_ONE_CODEPOINT(codepoint, token_type)             \
	case codepoint:                                            \
	{                                                          \
		std::ignore = advance(); /* Skip over the character */ \
		return create_token(token_type, current_token());      \
	}

#define MATCH_TWO_CODEPOINTS(first_codepoint, second_codepoint, one_match_type, two_match_type) \
	case first_codepoint:                                                                       \
	{                                                                                           \
		std::ignore = advance(); /* Skip over the first character */                            \
		if (peek_at(0) == (second_codepoint)) {                                                 \
			std::ignore = advance(); /* Skip over the second character */                       \
			return create_token(two_match_type, current_token());                               \
		}                                                                                       \
		return create_token(one_match_type, current_token());                                   \
	}

#define MATCH_TWO_CODEPOINTS_SPECIAL(                                                                            \
	first_codepoint, second_codepoint_a, second_codepoint_b, match_one_type, match_two_type_a, match_two_type_b) \
	case first_codepoint:                                                                                        \
	{                                                                                                            \
		std::ignore               = advance(); /* Skip over the first character */                               \
		const auto next_codepoint = peek_at(0);                                                                  \
		if (next_codepoint == (second_codepoint_a)) {                                                            \
			std::ignore = advance(); /* Skip over the second character */                                        \
			return create_token(match_two_type_a, current_token());                                              \
		}                                                                                                        \
		if (next_codepoint == (second_codepoint_b)) {                                                            \
			std::ignore = advance(); /* Skip over the second character */                                        \
			return create_token(match_two_type_b, current_token());                                              \
		}                                                                                                        \
		return create_token(match_one_type, current_token());                                                    \
	}

	using namespace std::string_view_literals;

	Lexer::Lexer(std::string_view script)
		: _script(script), _offset_start(0), _offset_current(0), _current_location(1, 0)
	{
	}

	std::vector<Token> Lexer::tokenize(std::string_view script) { return Lexer{ script }.tokenize(); }

	std::vector<Token> Lexer::tokenize()
	{
		std::vector<Token> result;
		if (_script.empty()) {
			return result;
		}

		for (;;) {
			auto token = scan_token();
			if (token.type == Token::Type::SpecialEndOfFile) {
				break;
			}
			result.push_back(std::move(token));
		}
		return result;
	}

	std::string_view Lexer::current_token(std::size_t exclude_start, std::size_t exclude_end)
	{
		return _script.substr(_offset_start + exclude_start,
		                      _offset_current - _offset_start - exclude_start - exclude_end);
	}

	Token Lexer::create_token(Token::Type type, std::string_view data)
	{
		auto location = SourceLocation{ _current_location.row,
			                            std::min(_current_location.column,
			                                     _current_location.column - static_cast<u32>(data.size())) };
		return Token{ type, data, std::move(location) };
	};

	Token Lexer::scan_token()
	{
		// Consume whitespace and comments.
		{
			auto keep_consuming = false;
			do {
				keep_consuming = false;
				advance_while(CodePoint::is_whitespace);

				// Skip comments.
				if (peek_at(0) == '#') {
					advance_while(std::not_fn(CodePoint::is_newline));
					keep_consuming = true;
				}
			} while (peek_at(0) != CodePoint::k_eof && keep_consuming);
		};

		_offset_start = _offset_current;

		auto current_codepoint = peek_at(0);
		if (current_codepoint == CodePoint::k_eof) {
			return create_token(Token::Type::SpecialEndOfFile, current_token());
		}

		// Numeric
		const bool has_sign = current_codepoint == CodePoint::k_hyphen || current_codepoint == CodePoint::k_plus_sign;
		if (CodePoint::is_digit(peek_at(has_sign ? 1 : 0))) {
			advance_while([](const auto c) -> bool {
				return CodePoint::is_digit(c) || c == CodePoint::k_full_stop || c == CodePoint::k_plus_sign
				    || c == CodePoint::k_hyphen;
			});
			const auto lexeme = current_token();
			const bool has_decimal_point
				= std::ranges::any_of(lexeme, [](const auto c) -> bool { return c == CodePoint::k_full_stop; });
			return create_token(has_decimal_point ? Token::Type::LiteralFloat : Token::Type::LiteralInteger, lexeme);
		}

		// Strings
		if (current_codepoint == '"') {
			std::ignore = advance();  // Skip '"'
			advance_while([](const auto c) -> bool { return c != '"'; });
			if (peek_at(0) == CodePoint::k_eof) {
				static constexpr auto k_error_message = "unterminated string literal; did you forget '\"'?"sv;
				return create_token(Token::Type::SpecialError, k_error_message);
			}
			std::ignore = advance();  // Skip '"'
			return create_token(Token::Type::LiteralString, current_token(1, 1));
		}

		// Keywords & Literals
		if (advance_if([](const auto c) -> bool { return CodePoint::is_identifier(c) || CodePoint::is_digit(c); })) {
			static constexpr std::array k_keywords = {
				// Keywords
				std::make_pair("break"sv, Token::Type::KeywordBreak),
				std::make_pair("cast"sv, Token::Type::KeywordCast),
				std::make_pair("continue"sv, Token::Type::KeywordContinue),
				std::make_pair("else"sv, Token::Type::KeywordElse),
				std::make_pair("false"sv, Token::Type::KeywordFalse),
				std::make_pair("fn"sv, Token::Type::KeywordFn),
				std::make_pair("for"sv, Token::Type::KeywordFor),
				std::make_pair("if"sv, Token::Type::KeywordIf),
				std::make_pair("let"sv, Token::Type::KeywordLet),
				std::make_pair("mut"sv, Token::Type::KeywordMut),
				std::make_pair("native"sv, Token::Type::KeywordNative),
				std::make_pair("null"sv, Token::Type::KeywordNull),
				std::make_pair("return"sv, Token::Type::KeywordReturn),
				std::make_pair("struct"sv, Token::Type::KeywordStruct),
				std::make_pair("true"sv, Token::Type::KeywordTrue),
				std::make_pair("while"sv, Token::Type::KeywordWhile),
			};
			const auto lexeme = current_token();
			const auto it     = std::ranges::find(k_keywords, lexeme, &decltype(k_keywords)::value_type::first);
			return create_token(it != std::end(k_keywords) ? it->second : Token::Type::LiteralIdentifier, lexeme);
		}

		// Symbols
		switch (current_codepoint) {
			// Simple cases, where there are no other characters in the sequence.
			MATCH_ONE_CODEPOINT('(', Token::Type::SymbolParenLeft)
			MATCH_ONE_CODEPOINT(')', Token::Type::SymbolParenRight)
			MATCH_ONE_CODEPOINT(',', Token::Type::SymbolComma)
			MATCH_ONE_CODEPOINT('.', Token::Type::SymbolDot)
			MATCH_ONE_CODEPOINT(';', Token::Type::SymbolSemicolon)
			MATCH_ONE_CODEPOINT('?', Token::Type::SymbolQuestionMark)
			MATCH_ONE_CODEPOINT('[', Token::Type::SymbolBracketLeft)
			MATCH_ONE_CODEPOINT(']', Token::Type::SymbolBracketRight)
			MATCH_ONE_CODEPOINT('^', Token::Type::SymbolCaret)
			MATCH_ONE_CODEPOINT('{', Token::Type::SymbolBraceLeft)
			MATCH_ONE_CODEPOINT('}', Token::Type::SymbolBraceRight)

			// Tokens which might have one other character in the sequence.
			MATCH_TWO_CODEPOINTS('%', '=', Token::Type::SymbolPercent, Token::Type::SymbolPercentEqual)
			MATCH_TWO_CODEPOINTS('!', '=', Token::Type::SymbolBang, Token::Type::SymbolBangEqual)
			MATCH_TWO_CODEPOINTS('&', '&', Token::Type::SymbolAmpersand, Token::Type::SymbolAmpersandAmpersand)
			MATCH_TWO_CODEPOINTS('*', '=', Token::Type::SymbolStar, Token::Type::SymbolStarEqual)
			MATCH_TWO_CODEPOINTS('/', '=', Token::Type::SymbolSlash, Token::Type::SymbolSlashEqual)
			MATCH_TWO_CODEPOINTS(':', ':', Token::Type::SymbolColon, Token::Type::SymbolColonColon)
			MATCH_TWO_CODEPOINTS('<', '=', Token::Type::SymbolLess, Token::Type::SymbolLessEqual)
			MATCH_TWO_CODEPOINTS('=', '=', Token::Type::SymbolEqual, Token::Type::SymbolEqualEqual)
			MATCH_TWO_CODEPOINTS('>', '=', Token::Type::SymbolGreater, Token::Type::SymbolGreaterEqual)
			MATCH_TWO_CODEPOINTS('|', '|', Token::Type::SymbolPipe, Token::Type::SymbolPipePipe)

			// Special cases, where the second character might be either present or vary.
			MATCH_TWO_CODEPOINTS_SPECIAL(
				'+', '+', '=', Token::Type::SymbolPlus, Token::Type::SymbolPlusPlus, Token::Type::SymbolPlusEqual)
			MATCH_TWO_CODEPOINTS_SPECIAL(
				'-', '-', '=', Token::Type::SymbolMinus, Token::Type::SymbolMinusMinus, Token::Type::SymbolMinusEqual)
			default:
				break;
		}

		static constexpr auto k_error_message = "unrecognized token"sv;
		return create_token(Token::Type::SpecialError, k_error_message);
	}

	CodePoint::ValueType Lexer::peek_at(std::size_t n) const
	{
		if (_offset_current + n >= _script.size()) {
			return CodePoint::k_eof;
		}
		return _script[_offset_current + n];
	}

	CodePoint::ValueType Lexer::advance()
	{
		if (CodePoint::is_newline(_script[_offset_current++])) {
			++_current_location.row;
			_current_location.column = 0;
		} else {
			++_current_location.column;
		}
		return peek_at(0);
	}
};  // namespace soul::lexer
