#pragma once

#include "AST/ast.h"
#include "Common/Diagnostic.h"
#include "Lexer/Token.h"
#include "Parser/TypeSpecifier.h"
#include "Soul.h"

#include <expected>
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
		std::string_view _script                               = {};
		std::string_view _module_name                          = {};
		std::vector<Diagnostic> _diagnostics                   = {};

		public:
		/**
		 * @brief Converts linear sequence of tokens into an Abstract Syntax Tree (AST).
		 * @param module_name Name of the module.
		 * @param tokens Tokens to be parsed.
		 * @return Module with parsed statements.
		 */
		[[nodiscard]] static ast::ASTNode::Dependency Parse(std::string_view module_name,
		                                                    std::string_view script,
		                                                    std::span<const Lexer::Token> tokens);

		private:
		Parser(std::string_view module_name, std::string_view script, std::span<const Lexer::Token> tokens);

		ast::ASTNode::Dependency Parse();
		ast::ASTNode::Dependency ParseStatement();
		ast::ASTNode::Dependency ParseExpression();
		ast::ASTNode::Dependency ParseExpression(Precedence precedence);

		ast::ASTNode::Dependency ParseBinary(ast::ASTNode::Dependency lhs);
		ast::ASTNode::Dependency ParseCast();
		ast::ASTNode::Dependency ParseForLoop();
		ast::ASTNode::Dependency ParseFunctionCall(ast::ASTNode::Dependency dependency);
		ast::ASTNode::Dependency ParseFunctionDeclaration();
		ast::ASTNode::Dependency ParseGrouping();
		ast::ASTNode::Dependency ParseIf();
		ast::ASTNode::Dependency ParseLiteral();
		ast::ASTNode::Dependency ParseLoopControl();
		ast::ASTNode::Dependency ParseReturn();
		ast::ASTNode::Dependency ParseStructDeclaration();
		ast::ASTNode::Dependency ParseUnary();
		ast::ASTNode::Dependency ParseVariableDeclaration();
		ast::ASTNode::Dependency ParseWhileLoop();

		ast::ASTNode::Dependencies ParseBlockStatement();
		ast::ASTNode::Dependency ParseInitializerList();
		ast::ASTNode::Dependency ParseParameterDeclaration();
		std::optional<TypeSpecifier> ParseTypeSpecifier();

		/**
		 * @brief Creates new Error node in the AST and resynchronizes the parser.
		 */
		ast::ASTNode::Dependency CreateError(Diagnostic diagnostic);

		std::optional<Token> Require(TokenType type);
		std::optional<Token> Require(std::span<const TokenType> types);
		std::optional<Token> Peek(std::ptrdiff_t n);
		bool Match(TokenType type);

		static PrecedenceRule PrecedenceRule(TokenType type) noexcept;

		/** @brief Returns current token or an explicit EOF one. */
		Token CurrentTokenOrDefault() const noexcept;

		template <typename T>
			requires(std::is_arithmetic_v<T>)
		constexpr std::expected<T, std::string> ParseIntegralValue(std::string_view data)
		{
			T value{};
			auto result = std::from_chars(data.begin(), data.end(), value);
			if (!result) {
				return std::unexpected(std::make_error_condition(result.ec).message());
			}
			return value;
		}
	};
}  // namespace Soul::Parser
