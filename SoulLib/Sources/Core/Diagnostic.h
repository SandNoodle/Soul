#pragma once

#include "Lexer/Token.h"
#include "Soul.h"

#include <limits>
#include <optional>
#include <string>
#include <string_view>

namespace Soul
{
	struct Diagnostic
	{
		public:
		using Index    = UInt32;
		using ViewType = std::string_view;

		static constexpr Index k_invalid_index = std::numeric_limits<Index>::max();

		private:
		std::string _message{};
		std::optional<std::string> _hint_message{ std::nullopt };

		public:
		Diagnostic(std::string formatted_message, std::optional<std::string> formatted_hint = std::nullopt);

		//  --- Lexer ---
		static Diagnostic LexerUnterminatedStringLiteral();
		static Diagnostic LexerInternalUnrecognizedToken(ViewType unexpected_identifier);

		// --- Parser ---
		static Diagnostic ParserUnexpectedSymbol(Lexer::TokenType expected_symbol, ViewType unexpected_identifier);
		static Diagnostic ParserUnexpectedFunctionIdentifier(ViewType unexpected_identifier);
		static Diagnostic ParserUnexpectedKeyword(Lexer::TokenType expected_keyword, ViewType unexpected_identifier);
		static Diagnostic ParserExpectedTypeSpecifier();

		static Diagnostic ParserInternalBinaryOperatorMissing(ViewType unexpected_symbol);
		static Diagnostic ParserInternalPrecedenceRuleMissing(ViewType precedence_rule_type);

		// --- ---
	};
}  // namespace Soul
