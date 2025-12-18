#include "Parser/Parser.h"

#include "AST/AST.h"

#include <algorithm>
#include <array>
#include <expected>
#include <format>
#include <utility>

namespace Soul::Parser
{
	using namespace Soul::AST;
	using namespace Soul::Lexer;
	using namespace Soul::Types;

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

	AST::ASTNode::Dependency Parser::Parse(std::string_view module_name, std::span<const Token> tokens)
	{
		return Parser{ module_name, tokens }.Parse();
	}

	AST::ASTNode::Dependency Parser::Parse()
	{
		if (_tokens.empty()) {
			return ModuleNode::Create(std::string(_module_name), {});
		}

		ASTNode::Dependencies statements{};
		for (const auto& token : _tokens) {
			if (token.type == Token::Type::SPECIAL_ERROR) {
				statements.emplace_back(ErrorNode::Create(ErrorNode::Message{ token.data }));
			}
		}
		if (!statements.empty()) {
			return ModuleNode::Create(std::string(_module_name), std::move(statements));
		}

		while (_current_token != std::end(_tokens)) {
			statements.emplace_back(ParseStatement());
		}
		return ModuleNode::Create(std::string(_module_name), std::move(statements));
	}

	ASTNode::Dependency Parser::ParseStatement()
	{
		// Statements
		switch (CurrentTokenOrDefault().type) {
			case Token::Type::KEYWORD_BREAK:
			case Token::Type::KEYWORD_CONTINUE:
				return ParseLoopControl();
			case Token::Type::KEYWORD_FN:
				return ParseFunctionDeclaration();
			case Token::Type::KEYWORD_FOR:
				return ParseForLoop();
			case Token::Type::KEYWORD_IF:
				return ParseIf();
			case Token::Type::KEYWORD_LET:
				return ParseVariableDeclaration();
			case Token::Type::KEYWORD_RETURN:
				return ParseReturn();
			case Token::Type::KEYWORD_STRUCT:
				return ParseStructDeclaration();
			case Token::Type::KEYWORD_WHILE:
				return ParseWhileLoop();
			case Token::Type::SYMBOL_BRACE_LEFT:
				return BlockNode::Create(ParseBlockStatement());
			default:
				break;
		}

		return ParseExpression();
	}

	ASTNode::Dependency Parser::ParseExpression()
	{
		// NOTE: Starting precedence has to be at least one higher than no precedence.
		return ParseExpression(static_cast<Parser::Precedence>(std::to_underlying(Parser::Precedence::None) + 1));
	}

	ASTNode::Dependency Parser::ParseExpression(Parser::Precedence precedence)
	{
		auto prefix_rule = GetPrecedenceRule(CurrentTokenOrDefault().type).prefix;
		if (!prefix_rule) [[unlikely]] {
			return CreateError(std::format("[INTERNAL] no prefix precedence rule for '{}' was specified.",
			                               Token::NameInternal(CurrentTokenOrDefault().type)));
		}

		auto prefix_expression = (this->*prefix_rule)();

		while (precedence <= GetPrecedenceRule(CurrentTokenOrDefault().type).precedence) {
			auto infix_rule = GetPrecedenceRule(CurrentTokenOrDefault().type).infix;
			if (!infix_rule) [[unlikely]] {
				return CreateError(std::format("[INTERNAL] no infix precedence rule for '{}' was specified.",
				                               Token::NameInternal(CurrentTokenOrDefault().type)));
			}
			prefix_expression = (this->*infix_rule)(std::move(prefix_expression));
		}

		return prefix_expression;
	}

	ASTNode::Dependency Parser::ParseBinary(ASTNode::Dependency lhs)
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
		auto binary_operator = Require(k_binary_operators);
		if (!binary_operator) {
			return CreateError(
				std::format("expected binary operator, but got: '{}'", std::string(CurrentTokenOrDefault().data)));
		}

		// <expression>
		auto precedence = GetPrecedenceRule(binary_operator->type).precedence;
		auto rhs        = ParseExpression(precedence);

		return BinaryNode::Create(std::move(lhs), std::move(rhs), ASTNode::AsOperator(binary_operator->type));
	}

	ASTNode::Dependency Parser::ParseCast()
	{
		// <cast_expression> ::= <keyword_cast> '<' <type_specifier> '>' '(' <expression> ')'

		// <keyword_cast>
		if (!Require(Token::Type::KEYWORD_CAST)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(Token::Type::KEYWORD_CAST),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// '<'
		if (!Require(Token::Type::SYMBOL_LESS)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_LESS),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <type_specifier>
		auto type_specifier = ParseTypeSpecifier();
		if (!type_specifier) {
			return CreateError("expected type specifier");
		}

		// '>'
		if (!Require(Token::Type::SYMBOL_GREATER)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_GREATER),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// '('
		if (!Require(Token::Type::SYMBOL_PAREN_LEFT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_PAREN_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <expression>
		auto expression = ParseExpression();

		// ')'
		if (!Require(Token::Type::SYMBOL_PAREN_RIGHT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_PAREN_RIGHT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		return CastNode::Create(std::move(expression), std::move(*type_specifier));
	}

	ASTNode::Dependency Parser::ParseForLoop()
	{
		// <for_loop> ::= <keyword_for> '(' [<expression>] ';' [ <expression> ] ';' [ <expression> ] ')'
		// <block_statement>

		// <keyword_for>
		if (!Require(Token::Type::KEYWORD_FOR)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(Token::Type::KEYWORD_FOR),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// '('
		if (!Require(Token::Type::SYMBOL_PAREN_LEFT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_PAREN_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// [Optional] <expression>
		ASTNode::Dependency initialization = nullptr;
		if (!Match(Token::Type::SYMBOL_SEMICOLON)) {
			initialization = ParseParameterDeclaration();

			// ';'
			if (!Require(Token::Type::SYMBOL_SEMICOLON)) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(Token::Type::SYMBOL_SEMICOLON),
				                               std::string(CurrentTokenOrDefault().data)));
			}
		}

		// [Optional] <expression>
		ASTNode::Dependency condition = nullptr;
		if (!Match(Token::Type::SYMBOL_SEMICOLON)) {
			condition = ParseExpression();

			// ';'
			if (!Require(Token::Type::SYMBOL_SEMICOLON)) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(Token::Type::SYMBOL_SEMICOLON),
				                               std::string(CurrentTokenOrDefault().data)));
			}
		}

		// [Optional] <expression>
		ASTNode::Dependency update = nullptr;
		if (!Match(Token::Type::SYMBOL_PAREN_RIGHT)) {
			update = ParseExpression();

			// ')'
			if (!Require(Token::Type::SYMBOL_PAREN_RIGHT)) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(Token::Type::SYMBOL_PAREN_RIGHT),
				                               std::string(CurrentTokenOrDefault().data)));
			}
		}

		// <block_statement>
		auto statements = ParseBlockStatement();

		return ForLoopNode::Create(std::move(initialization),
		                           std::move(condition),
		                           std::move(update),
		                           BlockNode::Create(std::move(statements)));
	}

	ASTNode::Dependency Parser::ParseFunctionCall(ASTNode::Dependency dependency)
	{
		// <function_call> ::= <identifier> [ '(' <parameter_declaration>, ... ')' ]

		// <identifier>
		if (!dependency->Is<LiteralNode>()) {
			const auto previous_token = Peek(-1);
			return CreateError(std::format("expected function name identifier, but got: '{}'",
			                               std::string(previous_token ? previous_token->data : "__ERROR__")));
		}

		// [Optional] '(' <parameter_list> ')'
		ASTNode::Dependencies parameters{};
		if (!Require(Token::Type::SYMBOL_PAREN_LEFT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_PAREN_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		const bool parenthesis_next = CurrentTokenOrDefault().type == Token::Type::SYMBOL_PAREN_RIGHT;
		if (!parenthesis_next) {
			do {
				// <expression>
				parameters.emplace_back(ParseExpression());
			} while (Match(Token::Type::SYMBOL_COMMA));
		}

		if (!Require(Token::Type::SYMBOL_PAREN_RIGHT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_PAREN_RIGHT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		return FunctionCallNode::Create(std::string(dependency->As<LiteralNode>().value.As<Identifier>()),
		                                std::move(parameters));
	}

	ASTNode::Dependency Parser::ParseFunctionDeclaration()
	{
		// <function_declaration> ::= <keyword_fn> <identifier> [ <parameter_declaration>, ... ] '::' <type_specifier>
		// <block_statement>

		// <keyword_fn>
		if (!Require(Token::Type::KEYWORD_FN)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(Token::Type::KEYWORD_FN),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <identifier>
		auto name_identifier = Require(Token::Type::LITERAL_IDENTIFIER);
		if (!name_identifier) {
			return CreateError(
				std::format("expected function identifier, but got: '{}'", std::string(CurrentTokenOrDefault().data)));
		}

		// [Optional] '(' <parameter_list> ')'
		ASTNode::Dependencies parameters{};
		if (Match(Token::Type::SYMBOL_PAREN_LEFT)) {
			const bool parenthesis_next = CurrentTokenOrDefault().type == Token::Type::SYMBOL_PAREN_RIGHT;
			if (!parenthesis_next) {
				do {
					parameters.emplace_back(ParseParameterDeclaration());
				} while (Match(Token::Type::SYMBOL_COMMA));
			}

			if (!Require(Token::Type::SYMBOL_PAREN_RIGHT)) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(Token::Type::SYMBOL_PAREN_RIGHT),
				                               std::string(CurrentTokenOrDefault().data)));
			}
		}

		// '::'
		if (!Require(Token::Type::SYMBOL_COLON_COLON)) {
			return CreateError(std::format("expected type separator '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_COLON_COLON),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <identifier>
		auto type_specifier = ParseTypeSpecifier();
		if (!type_specifier) {
			return CreateError("expected type specifier");
		}

		auto statements = ParseBlockStatement();

		return FunctionDeclarationNode::Create(std::string(name_identifier->data),
		                                       std::move(*type_specifier),
		                                       std::move(parameters),
		                                       BlockNode::Create(std::move(statements)));
	}

	AST::ASTNode::Dependency Parser::ParseGrouping()
	{
		// <grouping_expression> ::= '(' <expression> ')'

		// '('
		if (!Require(Token::Type::SYMBOL_PAREN_LEFT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_PAREN_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <expression>
		auto expression = ParseExpression();

		// ')'
		if (!Require(Token::Type::SYMBOL_PAREN_RIGHT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_PAREN_RIGHT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		return expression;
	}

	ASTNode::Dependency Parser::ParseIf()
	{
		// <if_statement> ::= <keyword_if> '(' <expression> ')' <block_statement> [ <keyword_else> <block_statement> ]

		// <keyword_if>
		if (!Require(Token::Type::KEYWORD_IF)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(Token::Type::KEYWORD_IF),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// '('
		if (!Require(Token::Type::SYMBOL_PAREN_LEFT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_PAREN_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <expression>
		auto condition = ParseExpression();

		// ')'
		if (!Require(Token::Type::SYMBOL_PAREN_RIGHT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_PAREN_RIGHT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <block_statement>
		auto true_statements = ParseBlockStatement();

		// [ <keyword_else> <block_statement> ]
		ASTNode::Dependencies false_statements{};
		if (Match(Token::Type::KEYWORD_ELSE)) {
			false_statements = ParseBlockStatement();
		}

		return IfNode::Create(std::move(condition),
		                      BlockNode::Create(std::move(true_statements)),
		                      BlockNode::Create(std::move(false_statements)));
	}

	ASTNode::Dependency Parser::ParseLiteral()
	{
		// <literal> ::= <integer_literal>  | <float_literal> | <string_literal> | <keyword_true> | <keyword_false>

		const auto& token = Require(std::array{ Token::Type::KEYWORD_FALSE,
		                                        Token::Type::KEYWORD_NULL,
		                                        Token::Type::KEYWORD_TRUE,
		                                        Token::Type::LITERAL_FLOAT,
		                                        Token::Type::LITERAL_IDENTIFIER,
		                                        Token::Type::LITERAL_INTEGER,
		                                        Token::Type::LITERAL_STRING });
		if (!token) {
			return CreateError(
				std::format("expected literal expression, but got: '{}'", Token::Name(CurrentTokenOrDefault().type)));
		}

		if (token->type == Token::Type::LITERAL_FLOAT) {
			auto result = ParseIntegralValue<Float64>(token->data);
			if (!result.has_value()) {
				return CreateError(std::format("failed to parse float expression, because: '{}'", result.error()));
			}
			if (*result <= std::numeric_limits<Float32>::lowest() || *result >= std::numeric_limits<Float32>::max()) {
				return LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::FLOAT64>(*result));
			}
			return LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::FLOAT32>(*result));
		}

		if (token->type == Token::Type::LITERAL_INTEGER) {
			auto result = ParseIntegralValue<Int64>(token->data);
			if (!result.has_value()) {
				return CreateError(std::format("failed to parse integer expression, because: '{}'", result.error()));
			}
			if (*result <= std::numeric_limits<Int32>::lowest() || *result >= std::numeric_limits<Int32>::max()) {
				return LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT64>(*result));
			}
			return LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(*result));
		}

		if (token->type == Token::Type::LITERAL_STRING) {
			return LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::STRING>(token->data));
		}

		if (token->type == Token::Type::LITERAL_IDENTIFIER) {
			return LiteralNode::Create(Identifier::create(token->data));
		}

		if (token->type == Token::Type::KEYWORD_TRUE) {
			return LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true));
		}

		if (token->type == Token::Type::KEYWORD_FALSE) {
			return LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(false));
		}

		if (token->type == Token::Type::KEYWORD_NULL) {
			return LiteralNode::Create({});
		}

		return ErrorNode::Create("[INTERNAL] unknown literal");
	}

	AST::ASTNode::Dependency Parser::ParseLoopControl()
	{  // <loop_control> ::= <keyword_break> | <keyword_continue>

		auto token = Require(std::array{ Token::Type::KEYWORD_BREAK, Token::Type::KEYWORD_CONTINUE });
		if (!token) {
			return CreateError(std::format("expected '{}' or '{}' keyword, but got: '{}'",
			                               Token::Name(Token::Type::KEYWORD_BREAK),
			                               Token::Name(Token::Type::KEYWORD_CONTINUE),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <keyword_break> | <keyword_continue>
		const auto control_type = token->type == Token::Type::KEYWORD_BREAK ? LoopControlNode::Type::BREAK
		                                                                    : LoopControlNode::Type::CONTINUE;

		return LoopControlNode::Create(control_type);
	}

	AST::ASTNode::Dependency Parser::ParseReturn()
	{
		// <return_statement> ::= <keyword_return> [ <expression> ]

		// <keyword_return>
		if (!Require(Token::Type::KEYWORD_RETURN)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(Token::Type::KEYWORD_RETURN),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// [ <expression> ]
		ASTNode::Dependency expression = nullptr;
		if (CurrentTokenOrDefault().type != Token::Type::SYMBOL_SEMICOLON) {
			expression = ParseExpression();
		}

		return ReturnNode::Create(std::move(expression));
	}

	ASTNode::Dependency Parser::ParseStructDeclaration()
	{
		// <struct_declaration> ::= <keyword_struct> <identifier> '{' <parameter_declaration> [',' ... ] '}'

		// <keyword_struct>
		if (!Require(Token::Type::KEYWORD_STRUCT)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(Token::Type::KEYWORD_STRUCT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <identifier>
		auto name_identifier = Require(Token::Type::LITERAL_IDENTIFIER);
		if (!name_identifier) {
			return CreateError(
				std::format("expected struct identifier, but got: '{}'", std::string(CurrentTokenOrDefault().data)));
		}

		// '{'
		if (!Require(Token::Type::SYMBOL_BRACE_LEFT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_BRACE_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		ASTNode::Dependencies parameters{};
		if (const auto current_type = CurrentTokenOrDefault().type; current_type != Token::Type::SYMBOL_BRACE_RIGHT) {
			if (current_type == Token::Type::SPECIAL_END_OF_FILE) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(Token::Type::SYMBOL_BRACE_RIGHT),
				                               std::string(CurrentTokenOrDefault().data)));
			}

			while (!Match(Token::Type::SYMBOL_BRACE_RIGHT)) {
				parameters.emplace_back(ParseParameterDeclaration());

				// ','
				if (CurrentTokenOrDefault().type != Token::Type::SYMBOL_BRACE_RIGHT
				    && !Require(Token::Type::SYMBOL_COMMA)) {
					return CreateError(std::format("expected '{}', but got: '{}'",
					                               Token::Name(Token::Type::SYMBOL_COMMA),
					                               std::string(CurrentTokenOrDefault().data)));
				}
			}

			// '}'
			const auto previous_token = Peek(-1);
			if (!previous_token || previous_token->type != Token::Type::SYMBOL_BRACE_RIGHT) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(Token::Type::SYMBOL_BRACE_RIGHT),
				                               std::string(CurrentTokenOrDefault().data)));
			}
		} else {
			// '}'
			if (!Require(Token::Type::SYMBOL_BRACE_RIGHT)) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(Token::Type::SYMBOL_BRACE_RIGHT),
				                               std::string(CurrentTokenOrDefault().data)));
			}
		};

		return StructDeclarationNode::Create(std::string(name_identifier->data), std::move(parameters));
	}

	ASTNode::Dependency Parser::ParseUnary()
	{
		// <unary_expression> ::= <unary_operator> <expression>

		// <unary_operator>
		static constexpr std::array k_unary_operators = {
			Token::Type::SYMBOL_BANG,
			Token::Type::SYMBOL_MINUS,
			Token::Type::SYMBOL_MINUS_MINUS,
			Token::Type::SYMBOL_PLUS_PLUS,
		};
		auto unary_operator = Require(k_unary_operators);
		if (!unary_operator) {
			return CreateError(
				std::format("expected unary operator, but got: '{}'", std::string(CurrentTokenOrDefault().data)));
		}

		// <expression>
		auto expression = ParseExpression(Precedence::Prefix);

		return UnaryNode::Create(std::move(expression), ASTNode::AsOperator(unary_operator->type));
	}

	ASTNode::Dependency Parser::ParseVariableDeclaration()
	{
		// <variable_declaration> ::= <keyword_let> [ <keyword_mut> ] <identifier> ':' <type_specifier> '=' <expression>

		// <keyword_let>
		if (!Require(Token::Type::KEYWORD_LET)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(Token::Type::KEYWORD_LET),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// [Optional] <keyword_mut>
		const bool is_mutable = Match(Token::Type::KEYWORD_MUT);

		// <identifier>
		auto name_identifier = Require(Token::Type::LITERAL_IDENTIFIER);
		if (!name_identifier) {
			return CreateError(
				std::format("expected variable identifier, but got: '{}'", std::string(CurrentTokenOrDefault().data)));
		}

		// ':'
		if (!Require(Token::Type::SYMBOL_COLON)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_COLON),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <type_specifier>
		auto type_specifier = ParseTypeSpecifier();
		if (!type_specifier) {
			return CreateError("expected type specifier");
		}

		// '='
		if (!Require(Token::Type::SYMBOL_EQUAL)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_EQUAL),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <expression>
		auto expression = ParseExpression();

		return VariableDeclarationNode::Create(
			std::string(name_identifier->data), std::move(*type_specifier), std::move(expression), is_mutable);
	}

	ASTNode::Dependency Parser::ParseWhileLoop()
	{
		// <while_loop> ::= <keyword_while> [ '(' <expression> ')' ] <block_statement>

		// <keyword_while>
		if (!Require(Token::Type::KEYWORD_WHILE)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(Token::Type::KEYWORD_WHILE),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// [Optional] '(' <expression> ')'
		ASTNode::Dependency condition{};
		if (Match(Token::Type::SYMBOL_PAREN_LEFT)) {
			// <expression>
			condition = ParseExpression();

			// ')'
			if (!Require(Token::Type::SYMBOL_PAREN_RIGHT)) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(Token::Type::SYMBOL_PAREN_RIGHT),
				                               std::string(CurrentTokenOrDefault().data)));
			}
		} else {
			condition = LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true));
		}

		// <block_statement>
		auto statements = ParseBlockStatement();

		return WhileNode::Create(std::move(condition), BlockNode::Create(std::move(statements)));
	}

	ASTNode::Dependencies Parser::ParseBlockStatement()
	{
		// <block_statement> ::= '{' [ <statement> ';' ... ] '}'

		ASTNode::Dependencies statements{};
		if (!Require(Token::Type::SYMBOL_BRACE_LEFT)) {
			statements.emplace_back(CreateError(std::format("expected '{}', but got: '{}'",
			                                                Token::Name(Token::Type::SYMBOL_BRACE_LEFT),
			                                                std::string(CurrentTokenOrDefault().data))));
			return statements;
		}

		while (!Match(Token::Type::SYMBOL_BRACE_RIGHT)) {
			if (CurrentTokenOrDefault().type == Token::Type::SPECIAL_END_OF_FILE) {
				break;
			}

			// NOTE: We might be looking at a stray semicolon(s). Consume until nothing is left.
			while (Match(Token::Type::SYMBOL_SEMICOLON)) {
				// ...
			}
			if (Match(Token::Type::SYMBOL_BRACE_RIGHT)) {
				break;
			}

			statements.emplace_back(ParseStatement());

			// NOTE: If a statement ends with a BlockNode then consumption of a semicolon is not required.
			const bool must_consume_semicolon = [](ASTNode::Reference node) -> bool {
				if (!node) [[unlikely]] {
					return false;
				}

				// NOTE: [Explicit] ErrorNodes are synchronized separately, thus we should not interfere.
				if (node->Is<ErrorNode>()) {
					return false;
				}

				const bool has_body_block = node->Is<BlockNode>() || node->Is<ForLoopNode>()
				                         || node->Is<ForeachLoopNode>() || node->Is<WhileNode>() || node->Is<IfNode>();
				if (!has_body_block) {
					return true;
				}

				return false;
			}(statements.back().get());

			if (must_consume_semicolon && CurrentTokenOrDefault().type != Token::Type::SYMBOL_BRACE_RIGHT) {
				// ';'
				if (!Require(Token::Type::SYMBOL_SEMICOLON)) {
					statements.emplace_back(CreateError(std::format("expected '{}', but got: '{}'",
					                                                Token::Name(Token::Type::SYMBOL_SEMICOLON),
					                                                std::string(CurrentTokenOrDefault().data))));
					return statements;
				}
			}
		}

		const auto previous_token = Peek(-1);
		if (!previous_token || previous_token->type != Token::Type::SYMBOL_BRACE_RIGHT) {
			statements.emplace_back(CreateError(std::format("expected '{}', but got: '{}'",
			                                                Token::Name(Token::Type::SYMBOL_BRACE_RIGHT),
			                                                std::string(CurrentTokenOrDefault().data))));
			return statements;
		}

		return statements;
	}

	ASTNode::Dependency Parser::ParseInitializerList()
	{
		// <initializer_list> ::= '{' [ <expression> [ ',' ...] ] '}'
		static constexpr auto k_delimiter = Token::Type::SYMBOL_COMMA;

		if (!Require(Token::Type::SYMBOL_BRACE_LEFT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_BRACE_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		ASTNode::Dependencies expressions{};
		while (!Match(Token::Type::SYMBOL_BRACE_RIGHT)) {
			if (CurrentTokenOrDefault().type == Token::Type::SPECIAL_END_OF_FILE) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(Token::Type::SYMBOL_BRACE_RIGHT),
				                               std::string(CurrentTokenOrDefault().data)));
			}

			expressions.emplace_back(ParseExpression());

			// We need to consume the ',' symbol only if the next statement is not a right brace...
			if (CurrentTokenOrDefault().type != Token::Type::SYMBOL_BRACE_RIGHT) {
				if (!Require(k_delimiter)) {
					return CreateError(std::format("expected '{}', but got: '{}'",
					                               Token::Name(k_delimiter),
					                               std::string(CurrentTokenOrDefault().data)));
				}
			}
		}

		return BlockNode::Create(std::move(expressions));
	}

	ASTNode::Dependency Parser::ParseParameterDeclaration()
	{
		// <parameter_declaration> ::= <identifier> ':' <type_specifier> [ '=' <expression> ]

		// <identifier>
		auto name_identifier = Require(Token::Type::LITERAL_IDENTIFIER);
		if (!name_identifier) {
			return CreateError(
				std::format("expected name identifier, but got: '{}'", std::string(CurrentTokenOrDefault().data)));
		}

		// ':'
		if (!Require(Token::Type::SYMBOL_COLON)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(Token::Type::SYMBOL_BRACE_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <type_specifier>
		auto type_specifier = ParseTypeSpecifier();
		if (!type_specifier) {
			return CreateError("expected type specifier");
		}

		// [ '=' <expression> ]
		ASTNode::Dependency expression = nullptr;
		if (Match(Token::Type::SYMBOL_EQUAL)) {
			expression = ParseExpression();
		}

		return VariableDeclarationNode::Create(
			std::string(name_identifier->data), std::move(*type_specifier), std::move(expression), false);
	}

	std::optional<TypeSpecifier> Parser::ParseTypeSpecifier()
	{
		// <type_specifier> ::= [ <pointer_type_specifier> ] <type_specifier> [ <array_type_specifier> ]
		//                    | <base_type_specifier>

		const auto current_token = Peek(0);
		if (!current_token) {
			return std::nullopt;  // [ERROR]: End of File!
		}

		// <base_type_specifier> ::= <identifier>
		if (current_token->type == Token::Type::LITERAL_IDENTIFIER) {
			Require(Token::Type::LITERAL_IDENTIFIER);
			return TypeSpecifier(BaseTypeSpecifier(std::string(current_token->data)));
		}

		// <array_type_specifier> ::=  '[' <type_specifier> [ ',' <integer_literal> ] ']'
		if (current_token->type == Token::Type::SYMBOL_BRACKET_LEFT) {
			// '['
			if (!Require(Token::Type::SYMBOL_BRACKET_LEFT)) {
				return std::nullopt;
			}
			auto type_specifier = ParseTypeSpecifier();
			if (!type_specifier) {
				return std::nullopt;
			}

			// [Optional] ','
			ArrayTypeSpecifier::Size array_size = ArrayTypeSpecifier::k_unbound_size;
			if (Match(Token::Type::SYMBOL_COMMA)) {
				// <integer_literal>
				const auto integer_literal = Require(Token::Type::LITERAL_INTEGER);
				if (!integer_literal) {
					return std::nullopt;
				}

				auto array_size_literal_result = ParseIntegralValue<Int64>(integer_literal->data);
				if (!array_size_literal_result.has_value()) {
					return std::nullopt;
				}
				array_size = array_size_literal_result.value();
			}

			// ']'
			if (!Require(Token::Type::SYMBOL_BRACKET_RIGHT)) {
				return std::nullopt;
			}
			return TypeSpecifier(ArrayTypeSpecifier(std::move(*type_specifier), array_size));
		}

		// <pointer_type_specifier> ::= '*' <type_specifier>
		if (current_token->type == Token::Type::SYMBOL_STAR) {
			Require(Token::Type::SYMBOL_STAR);
			auto type_specifier = ParseTypeSpecifier();
			if (!type_specifier) {
				return std::nullopt;
			}
			return TypeSpecifier(PointerTypeSpecifier(std::move(*type_specifier)));
		}

		// [INTERNAL]: invalid type specifier
		return std::nullopt;
	}

	ASTNode::Dependency Parser::CreateError(ErrorNode::Message error_message)
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

		return ErrorNode::Create(std::move(error_message));
	}

	std::optional<Token> Parser::Require(Token::Type type)
	{
		if (_current_token == std::end(_tokens) || _current_token->type != type) {
			return std::nullopt;
		}
		return *_current_token++;
	}

	std::optional<Token> Parser::Require(std::span<const Token::Type> types)
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

	std::optional<Token> Parser::Peek(std::ptrdiff_t n)
	{
		if ((_current_token + n) != std::end(_tokens)) [[likely]] {
			return *(_current_token + n);
		}
		return std::nullopt;
	}

	bool Parser::Match(Token::Type type)
	{
		if (_current_token == std::end(_tokens) || _current_token->type != type) {
			return false;
		}
		_current_token++;
		return true;
	}

	Parser::PrecedenceRule Parser::GetPrecedenceRule(Token::Type type) const noexcept
	{
		switch (type) {
			// Assignment
			case Token::Type::SYMBOL_EQUAL:
			case Token::Type::SYMBOL_PLUS_EQUAL:
			case Token::Type::SYMBOL_MINUS_EQUAL:
			case Token::Type::SYMBOL_STAR_EQUAL:
			case Token::Type::SYMBOL_SLASH_EQUAL:
			case Token::Type::SYMBOL_PERCENT_EQUAL:
				return { Parser::Precedence::Assign, nullptr, &Parser::ParseBinary };

			// Comparison
			case Token::Type::SYMBOL_LESS:
			case Token::Type::SYMBOL_LESS_EQUAL:
			case Token::Type::SYMBOL_GREATER:
			case Token::Type::SYMBOL_GREATER_EQUAL:
				return { Precedence::Compare, nullptr, &Parser::ParseBinary };

			// Logical
			case Token::Type::SYMBOL_BANG:
				return { Parser::Precedence::Prefix, &Parser::ParseUnary, nullptr };

			// Literals
			case Token::Type::LITERAL_FLOAT:
			case Token::Type::LITERAL_IDENTIFIER:
			case Token::Type::LITERAL_INTEGER:
			case Token::Type::LITERAL_STRING:
			case Token::Type::KEYWORD_TRUE:
			case Token::Type::KEYWORD_FALSE:
			case Token::Type::KEYWORD_NULL:
				return { Precedence::None, &Parser::ParseLiteral, nullptr };

			// Arithmetic
			case Token::Type::SYMBOL_MINUS:
				return { Parser::Precedence::Additive, &Parser::ParseUnary, &Parser::ParseBinary };
			case Token::Type::SYMBOL_PLUS:
				return { Parser::Precedence::Additive, nullptr, &Parser::ParseBinary };
			case Token::Type::SYMBOL_PERCENT:
			case Token::Type::SYMBOL_SLASH:
			case Token::Type::SYMBOL_STAR:
				return { Parser::Precedence::Multiplicative, nullptr, &Parser::ParseBinary };
			case Token::Type::SYMBOL_PLUS_PLUS:
			case Token::Type::SYMBOL_MINUS_MINUS:
				return { Parser::Precedence::Prefix, &Parser::ParseUnary, nullptr };

			// Other
			case Token::Type::KEYWORD_CAST:
				return { Precedence::None, &Parser::ParseCast, nullptr };
			case Token::Type::SYMBOL_PAREN_LEFT:
				return { Precedence::Postfix, &Parser::ParseGrouping, &Parser::ParseFunctionCall };
			case Token::Type::SYMBOL_BRACE_LEFT:
				return { Precedence::None, &Parser::ParseInitializerList, nullptr };
			default:
				break;
		}

		return { Precedence::None, nullptr, nullptr };  // No precedence.
	}

	Token Parser::CurrentTokenOrDefault() const noexcept
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
