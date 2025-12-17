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
		enum class Type : u8;

		public:
		Type type;
		std::string_view data;
		SourceOffset location;

		bool operator==(const Token& other) const noexcept;
		std::strong_ordering operator<=>(const Token& other) const noexcept;
		explicit operator std::string() const;

		static std::string_view name(Token::Type type) noexcept;
		static std::string_view internal_name(Token::Type type) noexcept;
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
	enum class Token::Type : u8
	{
		KeywordBreak,
		KeywordCast,
		KeywordContinue,
		KeywordElse,
		KeywordFalse,
		KeywordFn,
		KeywordFor,
		KeywordIf,
		KeywordLet,
		KeywordMut,
		KeywordNative,
		KeywordNull,
		KeywordReturn,
		KeywordStruct,
		KeywordTrue,
		KeywordWhile,

		LiteralFloat,
		LiteralIdentifier,
		LiteralInteger,
		LiteralString,

		SymbolAmpersand,
		SymbolAmpersandAmpersand,
		SymbolBang,
		SymbolBangEqual,
		SymbolBraceLeft,
		SymbolBraceRight,
		SymbolBracketLeft,
		SymbolBracketRight,
		SymbolCaret,
		SymbolColon,
		SymbolColonColon,
		SymbolComma,
		SymbolDot,
		SymbolEqual,
		SymbolEqualEqual,
		SymbolGreater,
		SymbolGreaterEqual,
		SymbolLess,
		SymbolLessEqual,
		SymbolMinus,
		SymbolMinusEqual,
		SymbolMinusMinus,
		SymbolPercent,
		SymbolPercentEqual,
		SymbolParenLeft,
		SymbolParenRight,
		SymbolPipe,
		SymbolPipePipe,
		SymbolPlus,
		SymbolPlusEqual,
		SymbolPlusPlus,
		SymbolQuestionMark,
		SymbolSemicolon,
		SymbolSlash,
		SymbolSlashEqual,
		SymbolStar,
		SymbolStarEqual,

		SpecialError,
		SpecialEndOfFile,
	};
}  // namespace Soul::Lexer
