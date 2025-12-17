#pragma once

#include "Common/SourceOffset.h"
#include "Core/Types.h"

#include <format>
#include <string>

namespace Soul::Lexer
{
	/**
	 * @brief Represents a single lexical token, otherwise known as Lexeme.
	 */
	struct Token
	{
		public:
		enum class Type : UInt8;

		public:
		Type type;
		std::string_view data;
		SourceOffset location;

		bool operator==(const Token& other) const noexcept;
		std::strong_ordering operator<=>(const Token& other) const noexcept;
		explicit operator std::string() const;

		static std::string_view Name(Type type) noexcept;
		static std::string_view NameInternal(Type type) noexcept;
	};

	template <typename T>
	constexpr T& operator<<(T& stream, const Token& token)
	{
		stream << static_cast<std::string>(token);
		return stream;
	}

	/**
	 * @brief Represents the `type` of the Token, i.e. does it describe a keyword, literal, normal or a special symbol.
	 */
	enum class Token::Type : UInt8
	{
		KEYWORD_BREAK,
		KEYWORD_CAST,
		KEYWORD_CONTINUE,
		KEYWORD_ELSE,
		KEYWORD_FALSE,
		KEYWORD_FN,
		KEYWORD_FOR,
		KEYWORD_IF,
		KEYWORD_LET,
		KEYWORD_MUT,
		KEYWORD_NATIVE,
		KEYWORD_NULL,
		KEYWORD_RETURN,
		KEYWORD_STRUCT,
		KEYWORD_TRUE,
		KEYWORD_WHILE,

		LITERAL_FLOAT,
		LITERAL_IDENTIFIER,
		LITERAL_INTEGER,
		LITERAL_STRING,

		SYMBOL_AMPERSAND,
		SYMBOL_AMPERSAND_AMPERSAND,
		SYMBOL_BANG,
		SYMBOL_BANG_EQUAL,
		SYMBOL_BRACE_LEFT,
		SYMBOL_BRACE_RIGHT,
		SYMBOL_BRACKET_LEFT,
		SYMBOL_BRACKET_RIGHT,
		SYMBOL_CARET,
		SYMBOL_COLON,
		SYMBOL_COLON_COLON,
		SYMBOL_COMMA,
		SYMBOL_DOT,
		SYMBOL_EQUAL,
		SYMBOL_EQUAL_EQUAL,
		SYMBOL_GREATER,
		SYMBOL_GREATER_EQUAL,
		SYMBOL_LESS,
		SYMBOL_LESS_EQUAL,
		SYMBOL_MINUS,
		SYMBOL_MINUS_EQUAL,
		SYMBOL_MINUS_MINUS,
		SYMBOL_PERCENT,
		SYMBOL_PERCENT_EQUAL,
		SYMBOL_PAREN_LEFT,
		SYMBOL_PAREN_RIGHT,
		SYMBOL_PIPE,
		SYMBOL_PIPE_PIPE,
		SYMBOL_PLUS,
		SYMBOL_PLUS_EQUAL,
		SYMBOL_PLUS_PLUS,
		SYMBOL_QUESTION_MARK,
		SYMBOL_SEMICOLON,
		SYMBOL_SLASH,
		SYMBOL_SLASH_EQUAL,
		SYMBOL_STAR,
		SYMBOL_STAR_EQUAL,

		SPECIAL_ERROR,
		SPECIAL_END_OF_FILE,
	};
}  // namespace Soul::Lexer
