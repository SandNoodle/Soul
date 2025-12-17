#pragma once

#include "AST/AST.h"
#include "Core/Types.h"
#include "Lexer/Token.h"
#include "Parser/TypeSpecifier.h"

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
		std::string_view _module_name                          = {};

		public:
		/**
		 * @brief Converts linear sequence of tokens into an Abstract Syntax Tree (AST).
		 * @param module_name Name of the module.
		 * @param tokens Tokens to be parsed.
		 * @return Module with parsed statements.
		 */
		[[nodiscard]] static AST::ASTNode::Dependency parse(std::string_view module_name,
		                                                    std::span<const Lexer::Token> tokens);

		private:
		Parser(std::string_view module_name, std::span<const Lexer::Token> tokens);

		AST::ASTNode::Dependency parse();
		AST::ASTNode::Dependency parse_statement();
		AST::ASTNode::Dependency parse_expression();
		AST::ASTNode::Dependency parse_expression(Precedence precedence);

		AST::ASTNode::Dependency parse_binary(AST::ASTNode::Dependency lhs);
		AST::ASTNode::Dependency parse_cast();
		AST::ASTNode::Dependency parse_for_loop();
		AST::ASTNode::Dependency parse_function_call(AST::ASTNode::Dependency dependency);
		AST::ASTNode::Dependency parse_function_declaration();
		AST::ASTNode::Dependency parse_grouping();
		AST::ASTNode::Dependency parse_if();
		AST::ASTNode::Dependency parse_literal();
		AST::ASTNode::Dependency parse_loop_control();
		AST::ASTNode::Dependency parse_return();
		AST::ASTNode::Dependency parse_struct_declaration();
		AST::ASTNode::Dependency parse_unary();
		AST::ASTNode::Dependency parse_variable_declaration();
		AST::ASTNode::Dependency parse_while_loop();

		AST::ASTNode::Dependencies parse_block_statement();
		AST::ASTNode::Dependency parse_initializer_list();
		AST::ASTNode::Dependency parse_parameter_declaration();
		std::optional<TypeSpecifier> parse_type_specifier();

		/**
		 * @brief Creates new Error node in the AST and resynchronizes the parser.
		 */
		AST::ASTNode::Dependency create_error(AST::ErrorNode::Message error_message);

		std::optional<Lexer::Token> require(Lexer::Token::Type type);
		std::optional<Lexer::Token> require(std::span<const Lexer::Token::Type> types);
		std::optional<Lexer::Token> peek(std::ptrdiff_t n);
		bool match(Lexer::Token::Type type);

		PrecedenceRule precedence_rule(Lexer::Token::Type type) const noexcept;

		/** @brief Returns current token or an explicit EOF one. */
		Lexer::Token current_token_or_default() const noexcept;

		template <typename T>
			requires(std::is_arithmetic_v<T>)
		constexpr std::expected<T, std::string> parse_integral_value(std::string_view data)
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
