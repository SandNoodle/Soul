#pragma once

#include "AST/AST.h"
#include "Core/Types.h"
#include "Lexer/Token.h"
#include "Parser/TypeSpecifier.h"

#include <span>
#include <string_view>

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
		std::string_view _module_name                          = {};

		public:
		/**
		 * @brief Converts linear sequence of tokens into an Abstract Syntax Tree (AST).
		 * @param module_name Name of the module.
		 * @param tokens Tokens to be parsed.
		 * @return Module with parsed statements.
		 */
		[[nodiscard]] static AST::ASTNode::Dependency Parse(std::string_view module_name,
		                                                    std::span<const Lexer::Token> tokens);

		private:
		Parser(std::string_view module_name, std::span<const Lexer::Token> tokens);

		AST::ASTNode::Dependency Parse();
		AST::ASTNode::Dependency ParseStatement();
		AST::ASTNode::Dependency ParseExpression();
		AST::ASTNode::Dependency ParseExpression(Precedence precedence);

		AST::ASTNode::Dependency ParseBinary(AST::ASTNode::Dependency&& lhs);
		AST::ASTNode::Dependency ParseCast();
		AST::ASTNode::Dependency ParseForLoop();
		AST::ASTNode::Dependency ParseFunctionCall(AST::ASTNode::Dependency&& dependency);
		AST::ASTNode::Dependency ParseFunctionDeclaration();
		AST::ASTNode::Dependency ParseGrouping();
		AST::ASTNode::Dependency ParseIf();
		AST::ASTNode::Dependency ParseLiteral();
		AST::ASTNode::Dependency ParseLoopControl();
		AST::ASTNode::Dependency ParseReturn();
		AST::ASTNode::Dependency ParseStructDeclaration();
		AST::ASTNode::Dependency ParseUnary();
		AST::ASTNode::Dependency ParseVariableDeclaration();
		AST::ASTNode::Dependency ParseWhileLoop();

		AST::ASTNode::Dependencies ParseBlockStatement();
		AST::ASTNode::Dependency ParseInitializerList();
		AST::ASTNode::Dependency ParseParameterDeclaration();
		std::optional<TypeSpecifier> ParseTypeSpecifier();

		/**
		 * @brief Creates new Error node in the AST and resynchronizes the parser.
		 */
		AST::ASTNode::Dependency CreateError(AST::ErrorNode::Message error_message);

		std::optional<Lexer::Token> Require(Lexer::Token::Type type);
		std::optional<Lexer::Token> Require(std::span<const Lexer::Token::Type> types);
		std::optional<Lexer::Token> Peek(std::ptrdiff_t n);
		bool Match(Lexer::Token::Type type);

		static PrecedenceRule GetPrecedenceRule(Lexer::Token::Type type) noexcept;

		/** @brief Returns current token or an explicit EOF one. */
		Lexer::Token CurrentTokenOrDefault() const noexcept;
	};
}  // namespace Soul::Parser
