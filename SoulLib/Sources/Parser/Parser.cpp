#include "Parser/Parser.h"

#include "AST/AST.h"

#include <algorithm>
#include <array>
#include <format>
#include <utility>

namespace Soul::Parser
{
	using namespace Soul::AST;
	using namespace Soul::Lexer;
	using namespace Soul::Types;

	/** @brief Operator precedence (LOWEST to HIGHEST) */
	enum class Parser::Precedence : UInt8
	{
		None,
		Assign,          // =
		Or,              // ||
		And,             // &&
		Equal,           // == !=
		Compare,         // < > <= >=
		Additive,        // + -
		Multiplicative,  // * /
		Prefix,          // ! -
		Postfix,         // ()
	};

	struct Parser::PrecedenceRule
	{
		public:
		typedef ASTNode::Dependency (Parser::*PrefixFn)();
		typedef ASTNode::Dependency (Parser::*InfixFn)(ASTNode::Dependency);

		public:
		Precedence precedence = Precedence::None;
		PrefixFn prefix       = nullptr;
		InfixFn infix         = nullptr;
	};

	Parser::Parser(std::string_view module_name, std::span<const Token> tokens)
		: _tokens(tokens), _current_token(_tokens.begin()), _module_name(module_name)
	{
	}

	AST::ASTNode::Dependency Parser::parse(std::string_view module_name, std::span<const Token> tokens)
	{
		return Parser{ module_name, tokens }.parse();
	}

	AST::ASTNode::Dependency Parser::parse()
	{
		if (_tokens.empty()) {
			return ModuleNode::create(std::string(_module_name), {});
		}

		ASTNode::Dependencies statements{};
		for (const auto& token : _tokens) {
			if (token.type == Token::Type::SPECIAL_ERROR) {
				statements.emplace_back(ErrorNode::create(ErrorNode::Message{ token.data }));
			}
		}
		if (!statements.empty()) {
			return ModuleNode::create(std::string(_module_name), std::move(statements));
		}

		while (_current_token != std::end(_tokens)) {
			statements.emplace_back(parse_statement());
		}
		return ModuleNode::create(std::string(_module_name), std::move(statements));
	}

	ASTNode::Dependency Parser::parse_statement()
	{
		// Statements
		switch (current_token_or_default().type) {
			case Token::Type::KEYWORD_BREAK:
			case Token::Type::KEYWORD_CONTINUE:
				return parse_loop_control();
			case Token::Type::KEYWORD_FN:
				return parse_function_declaration();
			case Token::Type::KEYWORD_FOR:
				return parse_for_loop();
			case Token::Type::KEYWORD_IF:
				return parse_if();
			case Token::Type::KEYWORD_LET:
				return parse_variable_declaration();
			case Token::Type::KEYWORD_RETURN:
				return parse_return();
			case Token::Type::KEYWORD_STRUCT:
				return parse_struct_declaration();
			case Token::Type::KEYWORD_WHILE:
				return parse_while_loop();
			case Token::Type::SYMBOL_BRACE_LEFT:
				return BlockNode::create(parse_block_statement());
			default:
				break;
		}

		return parse_expression();
	}

	ASTNode::Dependency Parser::parse_expression()
	{
		// NOTE: Starting precedence has to be at least one higher than no precedence.
		return parse_expression(static_cast<Parser::Precedence>(std::to_underlying(Parser::Precedence::None) + 1));
	}

	ASTNode::Dependency Parser::parse_expression(Parser::Precedence precedence)
	{
		auto prefix_rule = precedence_rule(current_token_or_default().type).prefix;
		if (!prefix_rule) [[unlikely]] {
			return create_error(std::format("[INTERNAL] no prefix precedence rule for '{}' was specified.",
			                                Token::NameInternal(current_token_or_default().type)));
		}

		auto prefix_expression = (this->*prefix_rule)();

		while (precedence <= precedence_rule(current_token_or_default().type).precedence) {
			auto infix_rule = precedence_rule(current_token_or_default().type).infix;
			if (!infix_rule) [[unlikely]] {
				return create_error(std::format("[INTERNAL] no infix precedence rule for '{}' was specified.",
				                                Token::NameInternal(current_token_or_default().type)));
			}
			prefix_expression = (this->*infix_rule)(std::move(prefix_expression));
		}

		return prefix_expression;
	}

	ASTNode::Dependency Parser::parse_binary(ASTNode::Dependency lhs)
	{
		// <binary_expression> ::= <expression> <binary_operator> <expression>

		// <binary_operator>
		static constexpr std::array k_binary_operators = {
			// Comparison operators
			Token::Type::SYMBOL_GREATER,
			Token::Type::SYMBOL_GREATER_EQUAL,
			Token::Type::SYMBOL_LESS,
			Token::Type::SYMBOL_LESS_EQUAL,
			Token::Type::SYMBOL_EQUAL_EQUAL,
			Token::Type::SYMBOL_BANG_EQUAL,

			// Assignment
			Token::Type::SYMBOL_EQUAL,
			Token::Type::SYMBOL_PLUS_EQUAL,
			Token::Type::SYMBOL_MINUS_EQUAL,
			Token::Type::SYMBOL_STAR_EQUAL,
			Token::Type::SYMBOL_SLASH_EQUAL,
			Token::Type::SYMBOL_PERCENT_EQUAL,

			// Arithmetic operators
			Token::Type::SYMBOL_PLUS,
			Token::Type::SYMBOL_MINUS,
			Token::Type::SYMBOL_STAR,
			Token::Type::SYMBOL_SLASH,
			Token::Type::SYMBOL_PERCENT,

			// Logical
			Token::Type::SYMBOL_AMPERSAND_AMPERSAND,
			Token::Type::SYMBOL_PIPE_PIPE,
		};
		auto binary_operator = require(k_binary_operators);
		if (!binary_operator) {
			return create_error(
				std::format("expected binary operator, but got: '{}'", std::string(current_token_or_default().data)));
		}

		// <expression>
		auto precedence = precedence_rule(binary_operator->type).precedence;
		auto rhs        = parse_expression(precedence);

		return BinaryNode::create(std::move(lhs), std::move(rhs), ASTNode::as_operator(binary_operator->type));
	}

	ASTNode::Dependency Parser::parse_cast()
	{
		// <cast_expression> ::= <keyword_cast> '<' <type_specifier> '>' '(' <expression> ')'

		// <keyword_cast>
		if (!require(Token::Type::KEYWORD_CAST)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::Name(Token::Type::KEYWORD_CAST),
			                                std::string(current_token_or_default().data)));
		}

		// '<'
		if (!require(Token::Type::SYMBOL_LESS)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_LESS),
			                                std::string(current_token_or_default().data)));
		}

		// <type_specifier>
		auto type_specifier = parse_type_specifier();
		if (!type_specifier) {
			return create_error("expected type specifier");
		}

		// '>'
		if (!require(Token::Type::SYMBOL_GREATER)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_GREATER),
			                                std::string(current_token_or_default().data)));
		}

		// '('
		if (!require(Token::Type::SYMBOL_PAREN_LEFT)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_PAREN_LEFT),
			                                std::string(current_token_or_default().data)));
		}

		// <expression>
		auto expression = parse_expression();

		// ')'
		if (!require(Token::Type::SYMBOL_PAREN_RIGHT)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_PAREN_RIGHT),
			                                std::string(current_token_or_default().data)));
		}

		return CastNode::create(std::move(expression), std::move(*type_specifier));
	}

	ASTNode::Dependency Parser::parse_for_loop()
	{
		// <for_loop> ::= <keyword_for> '(' [<expression>] ';' [ <expression> ] ';' [ <expression> ] ')'
		// <block_statement>

		// <keyword_for>
		if (!require(Token::Type::KEYWORD_FOR)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::Name(Token::Type::KEYWORD_FOR),
			                                std::string(current_token_or_default().data)));
		}

		// '('
		if (!require(Token::Type::SYMBOL_PAREN_LEFT)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_PAREN_LEFT),
			                                std::string(current_token_or_default().data)));
		}

		// [Optional] <expression>
		ASTNode::Dependency initialization = nullptr;
		if (!match(Token::Type::SYMBOL_SEMICOLON)) {
			initialization = parse_parameter_declaration();

			// ';'
			if (!require(Token::Type::SYMBOL_SEMICOLON)) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::Name(Token::Type::SYMBOL_SEMICOLON),
				                                std::string(current_token_or_default().data)));
			}
		}

		// [Optional] <expression>
		ASTNode::Dependency condition = nullptr;
		if (!match(Token::Type::SYMBOL_SEMICOLON)) {
			condition = parse_expression();

			// ';'
			if (!require(Token::Type::SYMBOL_SEMICOLON)) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::Name(Token::Type::SYMBOL_SEMICOLON),
				                                std::string(current_token_or_default().data)));
			}
		}

		// [Optional] <expression>
		ASTNode::Dependency update = nullptr;
		if (!match(Token::Type::SYMBOL_PAREN_RIGHT)) {
			update = parse_expression();

			// ')'
			if (!require(Token::Type::SYMBOL_PAREN_RIGHT)) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::Name(Token::Type::SYMBOL_PAREN_RIGHT),
				                                std::string(current_token_or_default().data)));
			}
		}

		// <block_statement>
		auto statements = parse_block_statement();

		return ForLoopNode::create(std::move(initialization),
		                           std::move(condition),
		                           std::move(update),
		                           BlockNode::create(std::move(statements)));
	}

	ASTNode::Dependency Parser::parse_function_call(ASTNode::Dependency dependency)
	{
		// <function_call> ::= <identifier> [ '(' <parameter_declaration>, ... ')' ]

		// <identifier>
		if (!dependency->is<LiteralNode>()) {
			const auto previous_token = peek(-1);
			return create_error(std::format("expected function name identifier, but got: '{}'",
			                                std::string(previous_token ? previous_token->data : "__ERROR__")));
		}

		// [Optional] '(' <parameter_list> ')'
		ASTNode::Dependencies parameters{};
		if (!require(Token::Type::SYMBOL_PAREN_LEFT)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_PAREN_LEFT),
			                                std::string(current_token_or_default().data)));
		}

		const bool parenthesis_next = current_token_or_default().type == Token::Type::SYMBOL_PAREN_RIGHT;
		if (!parenthesis_next) {
			do {
				// <expression>
				parameters.emplace_back(parse_expression());
			} while (match(Token::Type::SYMBOL_COMMA));
		}

		if (!require(Token::Type::SYMBOL_PAREN_RIGHT)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_PAREN_RIGHT),
			                                std::string(current_token_or_default().data)));
		}

		return FunctionCallNode::create(std::string(dependency->as<LiteralNode>().value.as<Identifier>()),
		                                std::move(parameters));
	}

	ASTNode::Dependency Parser::parse_function_declaration()
	{
		// <function_declaration> ::= <keyword_fn> <identifier> [ <parameter_declaration>, ... ] '::' <type_specifier>
		// <block_statement>

		// <keyword_fn>
		if (!require(Token::Type::KEYWORD_FN)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::Name(Token::Type::KEYWORD_FN),
			                                std::string(current_token_or_default().data)));
		}

		// <identifier>
		auto name_identifier = require(Token::Type::LITERAL_IDENTIFIER);
		if (!name_identifier) {
			return create_error(std::format("expected function identifier, but got: '{}'",
			                                std::string(current_token_or_default().data)));
		}

		// [Optional] '(' <parameter_list> ')'
		ASTNode::Dependencies parameters{};
		if (match(Token::Type::SYMBOL_PAREN_LEFT)) {
			const bool parenthesis_next = current_token_or_default().type == Token::Type::SYMBOL_PAREN_RIGHT;
			if (!parenthesis_next) {
				do {
					parameters.emplace_back(parse_parameter_declaration());
				} while (match(Token::Type::SYMBOL_COMMA));
			}

			if (!require(Token::Type::SYMBOL_PAREN_RIGHT)) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::Name(Token::Type::SYMBOL_PAREN_RIGHT),
				                                std::string(current_token_or_default().data)));
			}
		}

		// '::'
		if (!require(Token::Type::SYMBOL_COLON_COLON)) {
			return create_error(std::format("expected type separator '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_COLON_COLON),
			                                std::string(current_token_or_default().data)));
		}

		// <identifier>
		auto type_specifier = parse_type_specifier();
		if (!type_specifier) {
			return create_error("expected type specifier");
		}

		auto statements = parse_block_statement();

		return FunctionDeclarationNode::create(std::string(name_identifier->data),
		                                       std::move(*type_specifier),
		                                       std::move(parameters),
		                                       BlockNode::create(std::move(statements)));
	}

	AST::ASTNode::Dependency Parser::parse_grouping()
	{
		// <grouping_expression> ::= '(' <expression> ')'

		// '('
		if (!require(Token::Type::SYMBOL_PAREN_LEFT)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_PAREN_LEFT),
			                                std::string(current_token_or_default().data)));
		}

		// <expression>
		auto expression = parse_expression();

		// ')'
		if (!require(Token::Type::SYMBOL_PAREN_RIGHT)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_PAREN_RIGHT),
			                                std::string(current_token_or_default().data)));
		}

		return expression;
	}

	ASTNode::Dependency Parser::parse_if()
	{
		// <if_statement> ::= <keyword_if> '(' <expression> ')' <block_statement> [ <keyword_else> <block_statement> ]

		// <keyword_if>
		if (!require(Token::Type::KEYWORD_IF)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::Name(Token::Type::KEYWORD_IF),
			                                std::string(current_token_or_default().data)));
		}

		// '('
		if (!require(Token::Type::SYMBOL_PAREN_LEFT)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_PAREN_LEFT),
			                                std::string(current_token_or_default().data)));
		}

		// <expression>
		auto condition = parse_expression();

		// ')'
		if (!require(Token::Type::SYMBOL_PAREN_RIGHT)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_PAREN_RIGHT),
			                                std::string(current_token_or_default().data)));
		}

		// <block_statement>
		auto true_statements = parse_block_statement();

		// [ <keyword_else> <block_statement> ]
		ASTNode::Dependencies false_statements{};
		if (match(Token::Type::KEYWORD_ELSE)) {
			false_statements = parse_block_statement();
		}

		return IfNode::create(std::move(condition),
		                      BlockNode::create(std::move(true_statements)),
		                      BlockNode::create(std::move(false_statements)));
	}

	ASTNode::Dependency Parser::parse_literal()
	{
		// <literal> ::= <integer_literal>  | <float_literal> | <string_literal> | <keyword_true> | <keyword_false>

		const auto& token = require(std::array{ Token::Type::KEYWORD_FALSE,
		                                        Token::Type::KEYWORD_NULL,
		                                        Token::Type::KEYWORD_TRUE,
		                                        Token::Type::LITERAL_FLOAT,
		                                        Token::Type::LITERAL_IDENTIFIER,
		                                        Token::Type::LITERAL_INTEGER,
		                                        Token::Type::LITERAL_STRING });
		if (!token) {
			return create_error(std::format("expected literal expression, but got: '{}'",
			                                Token::Name(current_token_or_default().type)));
		}

		if (token->type == Token::Type::LITERAL_FLOAT) {
			auto result = parse_integral_value<Float64>(token->data);
			if (!result.has_value()) {
				return create_error(std::format("failed to parse float expression, because: '{}'", result.error()));
			}
			if (*result <= std::numeric_limits<Float32>::lowest() || *result >= std::numeric_limits<Float32>::max()) {
				return LiteralNode::create(Scalar::create<PrimitiveType::Kind::FLOAT64>(*result));
			}
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::FLOAT32>(*result));
		}

		if (token->type == Token::Type::LITERAL_INTEGER) {
			auto result = parse_integral_value<Int64>(token->data);
			if (!result.has_value()) {
				return create_error(std::format("failed to parse integer expression, because: '{}'", result.error()));
			}
			if (*result <= std::numeric_limits<Int32>::lowest() || *result >= std::numeric_limits<Int32>::max()) {
				return LiteralNode::create(Scalar::create<PrimitiveType::Kind::INT64>(*result));
			}
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::INT32>(*result));
		}

		if (token->type == Token::Type::LITERAL_STRING) {
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::STRING>(token->data));
		}

		if (token->type == Token::Type::LITERAL_IDENTIFIER) {
			return LiteralNode::create(Identifier::create(token->data));
		}

		if (token->type == Token::Type::KEYWORD_TRUE) {
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::BOOLEAN>(true));
		}

		if (token->type == Token::Type::KEYWORD_FALSE) {
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::BOOLEAN>(false));
		}

		if (token->type == Token::Type::KEYWORD_NULL) {
			return LiteralNode::create({});
		}

		return ErrorNode::create("[INTERNAL] unknown literal");
	}

	AST::ASTNode::Dependency Parser::parse_loop_control()
	{  // <loop_control> ::= <keyword_break> | <keyword_continue>

		auto token = require(std::array{ Token::Type::KEYWORD_BREAK, Token::Type::KEYWORD_CONTINUE });
		if (!token) {
			return create_error(std::format("expected '{}' or '{}' keyword, but got: '{}'",
			                                Token::Name(Token::Type::KEYWORD_BREAK),
			                                Token::Name(Token::Type::KEYWORD_CONTINUE),
			                                std::string(current_token_or_default().data)));
		}

		// <keyword_break> | <keyword_continue>
		const auto control_type = token->type == Token::Type::KEYWORD_BREAK ? LoopControlNode::Type::Break
		                                                                    : LoopControlNode::Type::Continue;

		return LoopControlNode::create(control_type);
	}

	AST::ASTNode::Dependency Parser::parse_return()
	{
		// <return_statement> ::= <keyword_return> [ <expression> ]

		// <keyword_return>
		if (!require(Token::Type::KEYWORD_RETURN)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::Name(Token::Type::KEYWORD_RETURN),
			                                std::string(current_token_or_default().data)));
		}

		// [ <expression> ]
		ASTNode::Dependency expression = nullptr;
		if (current_token_or_default().type != Token::Type::SYMBOL_SEMICOLON) {
			expression = parse_expression();
		}

		return ReturnNode::create(std::move(expression));
	}

	ASTNode::Dependency Parser::parse_struct_declaration()
	{
		// <struct_declaration> ::= <keyword_struct> <identifier> '{' <parameter_declaration> [',' ... ] '}'

		// <keyword_struct>
		if (!require(Token::Type::KEYWORD_STRUCT)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::Name(Token::Type::KEYWORD_STRUCT),
			                                std::string(current_token_or_default().data)));
		}

		// <identifier>
		auto name_identifier = require(Token::Type::LITERAL_IDENTIFIER);
		if (!name_identifier) {
			return create_error(
				std::format("expected struct identifier, but got: '{}'", std::string(current_token_or_default().data)));
		}

		// '{'
		if (!require(Token::Type::SYMBOL_BRACE_LEFT)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_BRACE_LEFT),
			                                std::string(current_token_or_default().data)));
		}

		ASTNode::Dependencies parameters{};
		if (const auto current_type = current_token_or_default().type;
		    current_type != Token::Type::SYMBOL_BRACE_RIGHT) {
			if (current_type == Token::Type::SPECIAL_END_OF_FILE) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::Name(Token::Type::SYMBOL_BRACE_RIGHT),
				                                std::string(current_token_or_default().data)));
			}

			while (!match(Token::Type::SYMBOL_BRACE_RIGHT)) {
				parameters.emplace_back(parse_parameter_declaration());

				// ','
				if (current_token_or_default().type != Token::Type::SYMBOL_BRACE_RIGHT
				    && !require(Token::Type::SYMBOL_COMMA)) {
					return create_error(std::format("expected '{}', but got: '{}'",
					                                Token::Name(Token::Type::SYMBOL_COMMA),
					                                std::string(current_token_or_default().data)));
				}
			}

			// '}'
			const auto previous_token = peek(-1);
			if (!previous_token || previous_token->type != Token::Type::SYMBOL_BRACE_RIGHT) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::Name(Token::Type::SYMBOL_BRACE_RIGHT),
				                                std::string(current_token_or_default().data)));
			}
		} else {
			// '}'
			if (!require(Token::Type::SYMBOL_BRACE_RIGHT)) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::Name(Token::Type::SYMBOL_BRACE_RIGHT),
				                                std::string(current_token_or_default().data)));
			}
		};

		return StructDeclarationNode::create(std::string(name_identifier->data), std::move(parameters));
	}

	ASTNode::Dependency Parser::parse_unary()
	{
		// <unary_expression> ::= <unary_operator> <expression>

		// <unary_operator>
		static constexpr std::array k_unary_operators = {
			Token::Type::SYMBOL_BANG,
			Token::Type::SYMBOL_MINUS,
			Token::Type::SYMBOL_MINUS_MINUS,
			Token::Type::SYMBOL_PLUS_PLUS,
		};
		auto unary_operator = require(k_unary_operators);
		if (!unary_operator) {
			return create_error(
				std::format("expected unary operator, but got: '{}'", std::string(current_token_or_default().data)));
		}

		// <expression>
		auto expression = parse_expression(Precedence::Prefix);

		return UnaryNode::create(std::move(expression), ASTNode::as_operator(unary_operator->type));
	}

	ASTNode::Dependency Parser::parse_variable_declaration()
	{
		// <variable_declaration> ::= <keyword_let> [ <keyword_mut> ] <identifier> ':' <type_specifier> '=' <expression>

		// <keyword_let>
		if (!require(Token::Type::KEYWORD_LET)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::Name(Token::Type::KEYWORD_LET),
			                                std::string(current_token_or_default().data)));
		}

		// [Optional] <keyword_mut>
		const bool is_mutable = match(Token::Type::KEYWORD_MUT);

		// <identifier>
		auto name_identifier = require(Token::Type::LITERAL_IDENTIFIER);
		if (!name_identifier) {
			return create_error(std::format("expected variable identifier, but got: '{}'",
			                                std::string(current_token_or_default().data)));
		}

		// ':'
		if (!require(Token::Type::SYMBOL_COLON)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_COLON),
			                                std::string(current_token_or_default().data)));
		}

		// <type_specifier>
		auto type_specifier = parse_type_specifier();
		if (!type_specifier) {
			return create_error("expected type specifier");
		}

		// '='
		if (!require(Token::Type::SYMBOL_EQUAL)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_EQUAL),
			                                std::string(current_token_or_default().data)));
		}

		// <expression>
		auto expression = parse_expression();

		return VariableDeclarationNode::create(
			std::string(name_identifier->data), std::move(*type_specifier), std::move(expression), is_mutable);
	}

	ASTNode::Dependency Parser::parse_while_loop()
	{
		// <while_loop> ::= <keyword_while> [ '(' <expression> ')' ] <block_statement>

		// <keyword_while>
		if (!require(Token::Type::KEYWORD_WHILE)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::Name(Token::Type::KEYWORD_WHILE),
			                                std::string(current_token_or_default().data)));
		}

		// [Optional] '(' <expression> ')'
		ASTNode::Dependency condition{};
		if (match(Token::Type::SYMBOL_PAREN_LEFT)) {
			// <expression>
			condition = parse_expression();

			// ')'
			if (!require(Token::Type::SYMBOL_PAREN_RIGHT)) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::Name(Token::Type::SYMBOL_PAREN_RIGHT),
				                                std::string(current_token_or_default().data)));
			}
		} else {
			condition = LiteralNode::create(Scalar::create<PrimitiveType::Kind::BOOLEAN>(true));
		}

		// <block_statement>
		auto statements = parse_block_statement();

		return WhileNode::create(std::move(condition), BlockNode::create(std::move(statements)));
	}

	ASTNode::Dependencies Parser::parse_block_statement()
	{
		// <block_statement> ::= '{' [ <statement> ';' ... ] '}'

		ASTNode::Dependencies statements{};
		if (!require(Token::Type::SYMBOL_BRACE_LEFT)) {
			statements.emplace_back(create_error(std::format("expected '{}', but got: '{}'",
			                                                 Token::Name(Token::Type::SYMBOL_BRACE_LEFT),
			                                                 std::string(current_token_or_default().data))));
			return statements;
		}

		while (!match(Token::Type::SYMBOL_BRACE_RIGHT)) {
			if (current_token_or_default().type == Token::Type::SPECIAL_END_OF_FILE) {
				break;
			}

			// NOTE: We might be looking at a stray semicolon(s). Consume until nothing is left.
			while (match(Token::Type::SYMBOL_SEMICOLON)) {
				// ...
			}
			if (match(Token::Type::SYMBOL_BRACE_RIGHT)) {
				break;
			}

			statements.emplace_back(parse_statement());

			// NOTE: If a statement ends with a BlockNode then consumption of a semicolon is not required.
			const bool must_consume_semicolon = [](ASTNode::Reference node) -> bool {
				if (!node) [[unlikely]] {
					return false;
				}

				// NOTE: [Explicit] ErrorNodes are synchronized separately, thus we should not interfere.
				if (node->is<ErrorNode>()) {
					return false;
				}

				const bool has_body_block = node->is<BlockNode>() || node->is<ForLoopNode>()
				                         || node->is<ForeachLoopNode>() || node->is<WhileNode>() || node->is<IfNode>();
				if (!has_body_block) {
					return true;
				}

				return false;
			}(statements.back().get());

			if (must_consume_semicolon && current_token_or_default().type != Token::Type::SYMBOL_BRACE_RIGHT) {
				// ';'
				if (!require(Token::Type::SYMBOL_SEMICOLON)) {
					statements.emplace_back(create_error(std::format("expected '{}', but got: '{}'",
					                                                 Token::Name(Token::Type::SYMBOL_SEMICOLON),
					                                                 std::string(current_token_or_default().data))));
					return statements;
				}
			}
		}

		const auto previous_token = peek(-1);
		if (!previous_token || previous_token->type != Token::Type::SYMBOL_BRACE_RIGHT) {
			statements.emplace_back(create_error(std::format("expected '{}', but got: '{}'",
			                                                 Token::Name(Token::Type::SYMBOL_BRACE_RIGHT),
			                                                 std::string(current_token_or_default().data))));
			return statements;
		}

		return statements;
	}

	ASTNode::Dependency Parser::parse_initializer_list()
	{
		// <initializer_list> ::= '{' [ <expression> [ ',' ...] ] '}'
		static constexpr auto k_delimiter = Token::Type::SYMBOL_COMMA;

		if (!require(Token::Type::SYMBOL_BRACE_LEFT)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_BRACE_LEFT),
			                                std::string(current_token_or_default().data)));
		}

		ASTNode::Dependencies expressions{};
		while (!match(Token::Type::SYMBOL_BRACE_RIGHT)) {
			if (current_token_or_default().type == Token::Type::SPECIAL_END_OF_FILE) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::Name(Token::Type::SYMBOL_BRACE_RIGHT),
				                                std::string(current_token_or_default().data)));
			}

			expressions.emplace_back(parse_expression());

			// We need to consume the ',' symbol only if the next statement is not a right brace...
			if (current_token_or_default().type != Token::Type::SYMBOL_BRACE_RIGHT) {
				if (!require(k_delimiter)) {
					return create_error(std::format("expected '{}', but got: '{}'",
					                                Token::Name(k_delimiter),
					                                std::string(current_token_or_default().data)));
				}
			}
		}

		return BlockNode::create(std::move(expressions));
	}

	ASTNode::Dependency Parser::parse_parameter_declaration()
	{
		// <parameter_declaration> ::= <identifier> ':' <type_specifier> [ '=' <expression> ]

		// <identifier>
		auto name_identifier = require(Token::Type::LITERAL_IDENTIFIER);
		if (!name_identifier) {
			return create_error(
				std::format("expected name identifier, but got: '{}'", std::string(current_token_or_default().data)));
		}

		// ':'
		if (!require(Token::Type::SYMBOL_COLON)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::Name(Token::Type::SYMBOL_BRACE_LEFT),
			                                std::string(current_token_or_default().data)));
		}

		// <type_specifier>
		auto type_specifier = parse_type_specifier();
		if (!type_specifier) {
			return create_error("expected type specifier");
		}

		// [ '=' <expression> ]
		ASTNode::Dependency expression = nullptr;
		if (match(Token::Type::SYMBOL_EQUAL)) {
			expression = parse_expression();
		}

		return VariableDeclarationNode::create(
			std::string(name_identifier->data), std::move(*type_specifier), std::move(expression), false);
	}

	std::optional<TypeSpecifier> Parser::parse_type_specifier()
	{
		// <type_specifier> ::= [ <pointer_type_specifier> ] <type_specifier> [ <array_type_specifier> ]
		//                    | <base_type_specifier>

		const auto current_token = peek(0);
		if (!current_token) {
			return std::nullopt;  // [ERROR]: End of File!
		}

		// <base_type_specifier> ::= <identifier>
		if (current_token->type == Token::Type::LITERAL_IDENTIFIER) {
			require(Token::Type::LITERAL_IDENTIFIER);
			return TypeSpecifier(BaseTypeSpecifier(std::string(current_token->data)));
		}

		// <array_type_specifier> ::=  '[' <type_specifier> [ ',' <integer_literal> ] ']'
		if (current_token->type == Token::Type::SYMBOL_BRACKET_LEFT) {
			// '['
			if (!require(Token::Type::SYMBOL_BRACKET_LEFT)) {
				return std::nullopt;
			}
			auto type_specifier = parse_type_specifier();
			if (!type_specifier) {
				return std::nullopt;
			}

			// [Optional] ','
			ArrayTypeSpecifier::Size array_size = ArrayTypeSpecifier::k_unbound_size;
			if (match(Token::Type::SYMBOL_COMMA)) {
				// <integer_literal>
				const auto integer_literal = require(Token::Type::LITERAL_INTEGER);
				if (!integer_literal) {
					return std::nullopt;
				}

				auto array_size_literal_result = parse_integral_value<Int64>(integer_literal->data);
				if (!array_size_literal_result.has_value()) {
					return std::nullopt;
				}
				array_size = array_size_literal_result.value();
			}

			// ']'
			if (!require(Token::Type::SYMBOL_BRACKET_RIGHT)) {
				return std::nullopt;
			}
			return TypeSpecifier(ArrayTypeSpecifier(std::move(*type_specifier), array_size));
		}

		// <pointer_type_specifier> ::= '*' <type_specifier>
		if (current_token->type == Token::Type::SYMBOL_STAR) {
			require(Token::Type::SYMBOL_STAR);
			auto type_specifier = parse_type_specifier();
			if (!type_specifier) {
				return std::nullopt;
			}
			return TypeSpecifier(PointerTypeSpecifier(std::move(*type_specifier)));
		}

		// [INTERNAL]: invalid type specifier
		return std::nullopt;
	}

	ASTNode::Dependency Parser::create_error(ErrorNode::Message error_message)
	{
		static constexpr std::array k_synchronization_tokens = {
			Token::Type::KEYWORD_ELSE,       Token::Type::KEYWORD_FN,         Token::Type::KEYWORD_FOR,
			Token::Type::KEYWORD_IF,         Token::Type::KEYWORD_LET,        Token::Type::KEYWORD_NATIVE,
			Token::Type::KEYWORD_STRUCT,     Token::Type::KEYWORD_WHILE,      Token::Type::SYMBOL_SEMICOLON,
			Token::Type::SYMBOL_BRACE_RIGHT, Token::Type::SYMBOL_PAREN_RIGHT,
		};
		while (_current_token != std::end(_tokens)) {
			if (std::ranges::contains(k_synchronization_tokens, _current_token->type)) {
				_current_token++;
				break;  // Synchronized.
			}
			_current_token++;
		}

		return ErrorNode::create(std::move(error_message));
	}

	std::optional<Token> Parser::require(Token::Type type)
	{
		if (_current_token == std::end(_tokens) || _current_token->type != type) {
			return std::nullopt;
		}
		return *_current_token++;
	}

	std::optional<Token> Parser::require(std::span<const Token::Type> types)
	{
		if (_current_token == std::end(_tokens)) {
			return std::nullopt;
		}

		for (const auto& type : types) {
			if (_current_token->type == type) {
				return *_current_token++;
			}
		}
		return std::nullopt;
	}

	std::optional<Token> Parser::peek(std::ptrdiff_t n)
	{
		if ((_current_token + n) != std::end(_tokens)) [[likely]] {
			return *(_current_token + n);
		}
		return std::nullopt;
	}

	bool Parser::match(Token::Type type)
	{
		if (_current_token == std::end(_tokens) || _current_token->type != type) {
			return false;
		}
		_current_token++;
		return true;
	}

	Parser::PrecedenceRule Parser::precedence_rule(Token::Type type) const noexcept
	{
		switch (type) {
			// Assignment
			case Token::Type::SYMBOL_EQUAL:
			case Token::Type::SYMBOL_PLUS_EQUAL:
			case Token::Type::SYMBOL_MINUS_EQUAL:
			case Token::Type::SYMBOL_STAR_EQUAL:
			case Token::Type::SYMBOL_SLASH_EQUAL:
			case Token::Type::SYMBOL_PERCENT_EQUAL:
				return { Parser::Precedence::Assign, nullptr, &Parser::parse_binary };

			// Comparison
			case Token::Type::SYMBOL_LESS:
			case Token::Type::SYMBOL_LESS_EQUAL:
			case Token::Type::SYMBOL_GREATER:
			case Token::Type::SYMBOL_GREATER_EQUAL:
				return { Precedence::Compare, nullptr, &Parser::parse_binary };

			// Logical
			case Token::Type::SYMBOL_BANG:
				return { Parser::Precedence::Prefix, &Parser::parse_unary, nullptr };

			// Literals
			case Token::Type::LITERAL_FLOAT:
			case Token::Type::LITERAL_IDENTIFIER:
			case Token::Type::LITERAL_INTEGER:
			case Token::Type::LITERAL_STRING:
			case Token::Type::KEYWORD_TRUE:
			case Token::Type::KEYWORD_FALSE:
			case Token::Type::KEYWORD_NULL:
				return { Precedence::None, &Parser::parse_literal, nullptr };

			// Arithmetic
			case Token::Type::SYMBOL_MINUS:
				return { Parser::Precedence::Additive, &Parser::parse_unary, &Parser::parse_binary };
			case Token::Type::SYMBOL_PLUS:
				return { Parser::Precedence::Additive, nullptr, &Parser::parse_binary };
			case Token::Type::SYMBOL_PERCENT:
			case Token::Type::SYMBOL_SLASH:
			case Token::Type::SYMBOL_STAR:
				return { Parser::Precedence::Multiplicative, nullptr, &Parser::parse_binary };
			case Token::Type::SYMBOL_PLUS_PLUS:
			case Token::Type::SYMBOL_MINUS_MINUS:
				return { Parser::Precedence::Prefix, &Parser::parse_unary, nullptr };

			// Other
			case Token::Type::KEYWORD_CAST:
				return { Precedence::None, &Parser::parse_cast, nullptr };
			case Token::Type::SYMBOL_PAREN_LEFT:
				return { Precedence::Postfix, &Parser::parse_grouping, &Parser::parse_function_call };
			case Token::Type::SYMBOL_BRACE_LEFT:
				return { Precedence::None, &Parser::parse_initializer_list, nullptr };
			default:
				break;
		}

		return { Precedence::None, nullptr, nullptr };  // No precedence.
	}

	Token Parser::current_token_or_default() const noexcept
	{
		if (_current_token == std::end(_tokens)) {
			auto last_token = _tokens.back();
			return Token{
				.type     = Token::Type::SPECIAL_END_OF_FILE,
				.data     = Token::NameInternal(Token::Type::SPECIAL_END_OF_FILE),
				.location = SourceOffset{ last_token.location.row,
                                         static_cast<UInt32>(last_token.location.column + last_token.data.size()) }
			};
		}
		return *_current_token;
	}

}  // namespace Soul::Parser
