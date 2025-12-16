#include "Lexer/Lexer.h"

#include "Core/StringSwitch.h"

namespace Soul::Lexer
{
#define MATCH_ONE_CODEPOINT(codepoint, token_type) \
	case codepoint:                                \
	{                                              \
		Advance(); /* Skip over the character */   \
		return CreateToken(TokenType::token_type); \
	}

#define MATCH_TWO_CODEPOINTS(first_codepoint, second_codepoint, one_match_type, two_match_type) \
	case first_codepoint:                                                                       \
	{                                                                                           \
		Advance(); /* Skip over the first character */                                          \
		if (Peek() == (second_codepoint)) {                                                     \
			Advance(); /* Skip over the second character */                                     \
			return CreateToken(TokenType::two_match_type);                                      \
		}                                                                                       \
		return CreateToken(TokenType::one_match_type);                                          \
	}

#define MATCH_TWO_CODEPOINTS_SPECIAL(first_codepoint,              \
                                     second_codepoint_a,           \
                                     second_codepoint_b,           \
                                     token_type_match_one,         \
                                     token_type_match_two_a,       \
                                     token_type_match_two_b)       \
	case first_codepoint:                                          \
	{                                                              \
		Advance(); /* Skip over the first character */             \
		Codepoint::ValueType next_codepoint = Peek();              \
		if (next_codepoint == (second_codepoint_a)) {              \
			Advance(); /* Skip over the second character */        \
			return CreateToken(TokenType::token_type_match_two_a); \
		}                                                          \
		if (next_codepoint == (second_codepoint_b)) {              \
			Advance(); /* Skip over the second character */        \
			return CreateToken(TokenType::token_type_match_two_b); \
		}                                                          \
		return CreateToken(TokenType::token_type_match_one);       \
	}

	Lexer::Lexer(StringView script) : _script(std::move(script)), _offset_begin(0), _offset_end(0) {}

	std::vector<Token> Lexer::Tokenize(StringView script) { return Lexer(std::move(script)).Tokenize(); }

	std::vector<Token> Lexer::Tokenize()
	{
		std::vector<Token> result;
		if (_script.IsEmpty()) {
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

		_offset_begin = _offset_end;

		// --- End of File ---
		Codepoint::ValueType current_codepoint = Peek();
		if (current_codepoint == Codepoint::k_eof) {
			return CreateToken(TokenType::TOKEN_SPECIAL_END_OF_FILE);
		}

		// Numeric
		if (Codepoint::IsDigit(Peek(Codepoint::IsSign(current_codepoint) ? 1 : 0))) {
			if (Codepoint::IsSign(current_codepoint)) {
				Advance();
			}
			AdvanceWhile(Codepoint::IsValidNumericContinuator);

			// NOTE: Assume that lexeme containing a dot (.) is a floating-point numeric literal candidate.
			TokenType numeric_type    = TokenType::TOKEN_LITERAL_INTEGER;
			StringView current_lexeme = CurrentLexeme();
			for (USize index = 0; index < current_lexeme.Size(); ++index) {
				if (current_lexeme[index] == '.') {
					numeric_type = TokenType::TOKEN_LITERAL_FLOAT;
					break;
				}
			}
			return CreateToken(numeric_type);
		}

		// --- Strings ---
		if (Codepoint::IsValidStringStarter(current_codepoint)) {
			Advance();  // Skip '"'
			AdvanceWhile([current_codepoint](const Codepoint::ValueType c) -> bool { return c != current_codepoint; });
			if (Peek() == Codepoint::k_eof) {
				return CreateError(Diagnostic::LexerUnterminatedStringLiteral());
			}
			Advance();  // Skip '"'
			return CreateToken(TokenType::TOKEN_LITERAL_STRING);
		}

		// --- Keywords & Literals ---
		if (Codepoint::IsValidIdentifierStarter(current_codepoint)) {
			AdvanceWhile(Codepoint::IsValidIdentifierContinuator);

			TokenType type = StringSwitch<TokenType>(CurrentLexeme())
			                     .Case("break"_sv, TokenType::TOKEN_KEYWORD_BREAK)
			                     .Case("cast"_sv, TokenType::TOKEN_KEYWORD_CAST)
			                     .Case("continue"_sv, TokenType::TOKEN_KEYWORD_CONTINUE)
			                     .Case("else"_sv, TokenType::TOKEN_KEYWORD_ELSE)
			                     .Case("false"_sv, TokenType::TOKEN_KEYWORD_FALSE)
			                     .Case("fn"_sv, TokenType::TOKEN_KEYWORD_FN)
			                     .Case("for"_sv, TokenType::TOKEN_KEYWORD_FOR)
			                     .Case("if"_sv, TokenType::TOKEN_KEYWORD_IF)
			                     .Case("let"_sv, TokenType::TOKEN_KEYWORD_LET)
			                     .Case("mut"_sv, TokenType::TOKEN_KEYWORD_MUT)
			                     .Case("native"_sv, TokenType::TOKEN_KEYWORD_NATIVE)
			                     .Case("null"_sv, TokenType::TOKEN_KEYWORD_NULL)
			                     .Case("return"_sv, TokenType::TOKEN_KEYWORD_RETURN)
			                     .Case("struct"_sv, TokenType::TOKEN_KEYWORD_STRUCT)
			                     .Case("true"_sv, TokenType::TOKEN_KEYWORD_TRUE)
			                     .Case("while"_sv, TokenType::TOKEN_KEYWORD_WHILE)
			                     .Default(TokenType::TOKEN_LITERAL_IDENTIFIER);
			return CreateToken(type);
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

		return CreateError(Diagnostic::LexerInternalUnrecognizedToken(std::to_string(current_codepoint)));
	}

	Token Lexer::CreateToken(const TokenType type) const
	{
		return Token{
			.offset = SourceOffset(_offset_begin, _offset_end),
			.type   = type,
		};
	}

	Token Lexer::CreateError(Diagnostic diagnostic)
	{
		_diagnostics.emplace_back(std::move(diagnostic));
		return CreateToken(TokenType::TOKEN_SPECIAL_ERROR);
	}

	StringView Lexer::CurrentLexeme() { return StringView(_script.Raw() + _offset_begin, _offset_end - _offset_begin); }

	Codepoint::ValueType Lexer::Peek(const std::size_t n) const
	{
		if (_offset_end + n >= _script.Size()) {
			return Codepoint::k_eof;
		}
		return _script[_offset_end + n];
	}

	Codepoint::ValueType Lexer::Advance()
	{
		++_offset_end;
		return Peek();
	}
};  // namespace Soul::Lexer
