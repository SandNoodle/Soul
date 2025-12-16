#include "Core/Diagnostic.h"

#include <format>

namespace Soul
{
	using namespace Soul::Lexer;

	Diagnostic::Diagnostic(std::string formatted_message, std::optional<std::string> formatted_hint)
		: _message(std::move(formatted_message)), _hint_message(std::move(formatted_hint))
	{
	}

	Diagnostic Diagnostic::LexerUnterminatedStringLiteral() { return Diagnostic("unterminated string literal"); }

	Diagnostic Diagnostic::LexerInternalUnrecognizedToken(ViewType unexpected_identifier)
	{
		return Diagnostic(std::format("[INTERNAL] unrecognized token '{}'", unexpected_identifier));
	}

	Diagnostic Diagnostic::ParserUnexpectedSymbol(TokenType expected_symbol, ViewType unexpected_identifier)
	{
		return Diagnostic(
			std::format("expected '{}', but got '{}'", Token::Name(expected_symbol), unexpected_identifier));
	}

	Diagnostic Diagnostic::ParserUnexpectedFunctionIdentifier(ViewType unexpected_identifier)
	{
		return Diagnostic(std::format("expected function name identifier, but got: '{}'", unexpected_identifier));
	}

	Diagnostic Diagnostic::ParserUnexpectedKeyword(TokenType expected_keyword, ViewType unexpected_identifier) {}

	Diagnostic Diagnostic::ParserExpectedTypeSpecifier()
	{
		return Diagnostic("expected type specifier");
	}

	Diagnostic Diagnostic::ParserInternalBinaryOperatorMissing(ViewType unexpected_symbol) {}

	Diagnostic Diagnostic::ParserInternalPrecedenceRuleMissing(ViewType precedence_rule_type) {}
}  // namespace Soul
