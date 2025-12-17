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
			if (token.type == Token::Type::SpecialEndOfFile) {
				break;
			}
			result.push_back(token);
		}
		return result;
	}

	std::string_view Lexer::CurrentLexeme(std::size_t exclude_start, std::size_t exclude_end)
	{
		return _script.substr(_offset_start + exclude_start,
		                      _offset_current - _offset_start - exclude_start - exclude_end);
	}

	Token Lexer::CreateToken(Token::Type type, std::string_view data)
	{
		SourceOffset location = SourceOffset{ _current_location.row,
			                                  std::min(_current_location.column,
			                                           _current_location.column - static_cast<UInt32>(data.size())) };
		return Token{ type, data, location };
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
				if (Peek(0) == '#') {
					AdvanceWhile(std::not_fn(Codepoint::IsNewline));
					keep_consuming = true;
				}
			} while (Peek(0) != Codepoint::k_eof && keep_consuming);
		};

		_offset_start = _offset_current;

		Codepoint::ValueType current_codepoint = Peek(0);
		if (current_codepoint == Codepoint::k_eof) {
			return CreateToken(Token::Type::SpecialEndOfFile, CurrentLexeme());
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
			return CreateToken(has_decimal_point ? Token::Type::LiteralFloat : Token::Type::LiteralInteger, lexeme);
		}

		// Strings
		if (current_codepoint == '"') {
			Advance();  // Skip '"'
			AdvanceWhile([](const auto c) -> bool { return c != '"'; });
			if (Peek(0) == Codepoint::k_eof) {
				static constexpr auto k_error_message = "unterminated string literal; did you forget '\"'?"sv;
				return CreateToken(Token::Type::SpecialError, k_error_message);
			}
			Advance();  // Skip '"'
			return CreateToken(Token::Type::LiteralString, CurrentLexeme(1, 1));
		}

		// Keywords & Literals
		if (AdvanceIf([](const auto c) -> bool { return Codepoint::IsIdentifier(c) || Codepoint::IsDigit(c); })) {
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
			const auto lexeme = CurrentLexeme();
			const auto it     = std::ranges::find(k_keywords, lexeme, &decltype(k_keywords)::value_type::first);
			return CreateToken(it != std::end(k_keywords) ? it->second : Token::Type::LiteralIdentifier, lexeme);
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
		return CreateToken(Token::Type::SpecialError, k_error_message);
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
		return Peek(0);
	}
};  // namespace Soul::Lexer
