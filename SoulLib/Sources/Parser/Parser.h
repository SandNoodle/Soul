#pragma once

#include "AST/AST.h"
#include "Core/Diagnostic.h"
#include "Lexer/Token.h"
#include "Parser/TypeSpecifier.h"
#include "Soul.h"

#include <span>
#include <string_view>
#include <vector>

namespace Soul::Parser
{
	/**
	 * @brief Parser performs syntactic analysis on a given linear list of lexical tokens, i.e. converts it into an
	 * Abstract Syntax Tree (AST).
	 */
	class Parser
	{
		private:
		struct PrecedenceRule;
		enum class Precedence : UInt8;

		private:
		std::span<const Lexer::Token> _tokens                  = {};
		std::span<const Lexer::Token>::iterator _current_token = {};
		std::string_view _script                               = {};
		AST::ASTModule _module                                 = {};

		public:
		/**
		 * @brief Converts linear sequence of tokens into an Abstract Syntax Tree (AST).
		 * @param module_name Name of the module.
		 * @param script Script from which the tokens were lexed.
		 * @param tokens Tokens to be parsed.
		 * @return Module with parsed statements.
		 */
		[[nodiscard]] static AST::ASTModule Parse(std::string_view module_name,
		                                          std::string_view script,
		                                          std::span<const Lexer::Token> tokens);

		private:
		Parser(std::string_view module_name, std::string_view script, std::span<const Lexer::Token> tokens);

		AST::ASTModule Parse();
		AST::NodeIndex ParseStatement();
		AST::NodeIndex ParseExpression();
		AST::NodeIndex ParseExpression(Precedence precedence);

		AST::NodeIndex ParseBinary(AST::NodeIndex lhs);
		AST::NodeIndex ParseCast();
		AST::NodeIndex ParseForLoop();
		AST::NodeIndex ParseFunctionCall(AST::NodeIndex dependency);
		AST::NodeIndex ParseFunctionDeclaration();
		AST::NodeIndex ParseGrouping();
		AST::NodeIndex ParseIf();
		AST::NodeIndex ParseLiteral();
		AST::NodeIndex ParseLoopControl();
		AST::NodeIndex ParseReturn();
		AST::NodeIndex ParseStructDeclaration();
		AST::NodeIndex ParseUnary();
		AST::NodeIndex ParseVariableDeclaration();
		AST::NodeIndex ParseWhileLoop();

		Dependencies ParseBlockStatement();
		AST::NodeIndex ParseInitializerList();
		AST::NodeIndex ParseParameterDeclaration();
		std::optional<TypeSpecifier> ParseTypeSpecifier();

		/**
		 * @brief Creates new Error node in the AST and resynchronizes the parser.
		 */
		AST::NodeIndex CreateError(Diagnostic diagnostic);

		std::optional<Lexer::Token> Require(TokenType type);
		std::optional<Lexer::Token> Require(std::span<const TokenType> types);
		std::optional<Lexer::Token> Peek(std::ptrdiff_t n);
		bool Match(TokenType type);

		static PrecedenceRule PrecedenceRule(TokenType type) noexcept;

		/** @brief Returns current token or an explicit EOF one. */
		Lexer::Token CurrentTokenOrDefault() const noexcept;

		template <typename T, typename... Args>
		constexpr AST::NodeIndex Create(Args&&... args);
	};

	template <typename T, typename... Args>
	constexpr AST::NodeIndex Parser::Create(Args&&... args)
	{
	}
}  // namespace Soul::Parser
