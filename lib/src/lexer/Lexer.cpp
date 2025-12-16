#include "lexer/Lexer.h"

#include <algorithm>
#include <array>
#include <functional>

namespace soul::lexer
{
#define MATCH_ONE_CODEPOINT(codepoint, token_type)      \
	case codepoint:                                     \
	{                                                   \
		Advance(); /* Skip over the character */        \
		return CreateToken(token_type, CurrentToken()); \
	}

#define MATCH_TWO_CODEPOINTS(first_codepoint, second_codepoint, one_match_type, two_match_type) \
	case first_codepoint:                                                                       \
	{                                                                                           \
		Advance(); /* Skip over the first character */                                          \
		if (Peek() == (second_codepoint)) {                                                     \
			Advance(); /* Skip over the second character */                                     \
			return CreateToken(two_match_type, CurrentToken());                                 \
		}                                                                                       \
		return CreateToken(one_match_type, CurrentToken());                                     \
	}

#define MATCH_TWO_CODEPOINTS_SPECIAL(first_codepoint,                   \
                                     second_codepoint_a,                \
                                     second_codepoint_b,                \
                                     token_type_match_one,              \
                                     token_type_match_two_a,            \
                                     token_type_match_two_b)            \
	case first_codepoint:                                               \
	{                                                                   \
		Advance(); /* Skip over the first character */                  \
		Codepoint::ValueType next_codepoint = Peek();                   \
		if (next_codepoint == (second_codepoint_a)) {                   \
			Advance(); /* Skip over the second character */             \
			return CreateToken(token_type_match_two_a, CurrentToken()); \
		}                                                               \
		if (next_codepoint == (second_codepoint_b)) {                   \
			Advance(); /* Skip over the second character */             \
			return CreateToken(token_type_match_two_b, CurrentToken()); \
		}                                                               \
		return CreateToken(token_type_match_one, CurrentToken());       \
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
			result.push_back(std::move(token));
		}
		return result;
	}

	std::string_view Lexer::CurrentToken(std::size_t exclude_start, std::size_t exclude_end) const
	{
		return _script.substr(_offset_start + exclude_start,
		                      _offset_current - _offset_start - exclude_start - exclude_end);
	}

	Token Lexer::CreateToken(Token::Type type, std::string_view data) const
	{
		SourceLocation location = SourceLocation{ _current_location.row,
			                                      std::min(_current_location.column,
			                                               _current_location.column - static_cast<u32>(data.size())) };
		return Token{ type, data, std::move(location) };
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
			return CreateToken(Token::Type::SpecialEndOfFile, CurrentToken());
		}

		// Numeric
		if (Codepoint::IsDigit(Peek(Codepoint::IsSign(current_codepoint) ? 1 : 0))) {
			if (Codepoint::IsSign(current_codepoint)) {
				Advance();
			}
			AdvanceWhile(Codepoint::IsValidNumericContinuator);

			// NOTE: Assume that lexeme containing a dot (.) is a floating-point numeric literal candidate.
			Token::Type numeric_type        = Token::Type::LiteralInteger;
			std::string_view current_lexeme = CurrentToken();
			if (current_lexeme.contains('.')) {
				numeric_type = Token::Type::LiteralFloat;
			}
			return CreateToken(numeric_type, current_lexeme);
		}

		// --- Strings ---
		if (Codepoint::IsValidStringStarter(current_codepoint)) {
			Advance();  // Skip '"'
			AdvanceWhile([](const Codepoint::ValueType c) -> bool { return c != '"'; });
			if (Peek() == Codepoint::k_eof) {
				static constexpr auto k_error_message = "unterminated string literal; did you forget '\"'?"sv;
				return CreateToken(Token::Type::SpecialError, k_error_message);
			}
			Advance();  // Skip '"'
			return CreateToken(Token::Type::LiteralString, CurrentToken(1, 1));
		}

		// --- Keywords & Literals ---
		if (Codepoint::IsValidIdentifierStarter(current_codepoint)) {
			AdvanceWhile(Codepoint::IsValidIdentifierContinuator);

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
			std::string_view lexeme = CurrentToken();
			const auto it           = std::ranges::find(k_keywords, lexeme, &decltype(k_keywords)::value_type::first);
			return CreateToken(it != std::end(k_keywords) ? it->second : Token::Type::LiteralIdentifier, lexeme);
		}

		// --- Symbols ---
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
		return Peek();
	}
};  // namespace soul::lexer
