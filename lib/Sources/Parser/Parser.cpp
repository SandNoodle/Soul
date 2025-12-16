#include "Parser/Parser.h"

#include "AST/ast.h"

#include <algorithm>
#include <array>
#include <format>

namespace Soul::Parser
{
	using namespace Soul::ast;
	using namespace Soul::types;

	/** @brief Operator precedence (LOWEST to HIGHEST) */
	enum class Parser::Precedence : UInt8
	{
		PRECEDENCE_NONE,
		PRECEDENCE_ASSIGN,          // =
		PRECEDENCE_OR,              // ||
		PRECEDENCE_AND,             // &&
		PRECEDENCE_EQUAL,           // == !=
		PRECEDENCE_COMPARE,         // < > <= >=
		PRECEDENCE_ADDITIVE,        // + -
		PRECEDENCE_MULTIPLICATIVE,  // * /
		PRECEDENCE_PREFIX,          // ! -
		PRECEDENCE_POSTFIX,         // ()
	};

	struct Parser::PrecedenceRule
	{
		public:
		typedef ASTNode::Dependency (Parser::*PrefixFn)();
		typedef ASTNode::Dependency (Parser::*InfixFn)(ASTNode::Dependency);

		public:
		Precedence precedence = Precedence::PRECEDENCE_NONE;
		PrefixFn prefix       = nullptr;
		InfixFn infix         = nullptr;
	};

	Parser::Parser(std::string_view module_name, std::string_view script, std::span<const Token> tokens)
		: _tokens(tokens), _current_token(_tokens.begin()), _module_name(module_name)
	{
	}

	ASTNode::Dependency Parser::Parse(std::string_view module_name,
	                                  std::string_view script,
	                                  std::span<const Token> tokens)
	{
		return Parser{ module_name, script, tokens }.Parse();
	}

	ASTNode::Dependency Parser::Parse()
	{
		if (_tokens.empty()) {
			return ModuleNode::create(std::string(_module_name), {});
		}

		ASTNode::Dependencies statements{};
		for (const auto& token : _tokens) {
			if (token.type == TokenType::TOKEN_SPECIAL_ERROR) {
				statements.emplace_back(ErrorNode::create(ErrorNode::Message{ token.data }));
			}
		}
		if (!statements.empty()) {
			return ModuleNode::create(std::string(_module_name), std::move(statements));
		}

		while (_current_token != std::end(_tokens)) {
			statements.emplace_back(ParseStatement());
		}
		return ModuleNode::create(std::string(_module_name), std::move(statements));
	}

	ASTNode::Dependency Parser::ParseStatement()
	{
		// Statements
		switch (CurrentTokenOrDefault().type) {
			case TokenType::TOKEN_KEYWORD_BREAK:
			case TokenType::TOKEN_KEYWORD_CONTINUE:
				return ParseLoopControl();
			case TokenType::TOKEN_KEYWORD_FN:
				return ParseFunctionDeclaration();
			case TokenType::TOKEN_KEYWORD_FOR:
				return ParseForLoop();
			case TokenType::TOKEN_KEYWORD_IF:
				return ParseIf();
			case TokenType::TOKEN_KEYWORD_LET:
				return ParseVariableDeclaration();
			case TokenType::TOKEN_KEYWORD_RETURN:
				return ParseReturn();
			case TokenType::TOKEN_KEYWORD_STRUCT:
				return ParseStructDeclaration();
			case TokenType::TOKEN_KEYWORD_WHILE:
				return ParseWhileLoop();
			case TokenType::TOKEN_SYMBOL_BRACE_LEFT:
				return BlockNode::create(ParseBlockStatement());
			default:
				break;
		}

		return ParseExpression();
	}

	ASTNode::Dependency Parser::ParseExpression()
	{
		// NOTE: Starting precedence has to be at least one higher than no precedence.
		return ParseExpression(static_cast<Precedence>(std::to_underlying(Precedence::PRECEDENCE_NONE) + 1));
	}

	ASTNode::Dependency Parser::ParseExpression(Precedence precedence)
	{
		auto prefix_rule = PrecedenceRule(CurrentTokenOrDefault().type).prefix;
		if (!prefix_rule) [[unlikely]] {
			return CreateError(std::format("[INTERNAL] no prefix precedence rule for '{}' was specified.",
			                               Token::NameInternal(CurrentTokenOrDefault().type)));
		}

		auto prefix_expression = (this->*prefix_rule)();

		while (precedence <= PrecedenceRule(CurrentTokenOrDefault().type).precedence) {
			auto infix_rule = PrecedenceRule(CurrentTokenOrDefault().type).infix;
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
			TokenType::TOKEN_SYMBOL_GREATER,
			TokenType::TOKEN_SYMBOL_GREATER_EQUAL,
			TokenType::TOKEN_SYMBOL_LESS,
			TokenType::TOKEN_SYMBOL_LESS_EQUAL,
			TokenType::TOKEN_SYMBOL_EQUAL_EQUAL,
			TokenType::TOKEN_SYMBOL_BANG_EQUAL,

			// Assignment
			TokenType::TOKEN_SYMBOL_EQUAL,
			TokenType::TOKEN_SYMBOL_PLUS_EQUAL,
			TokenType::TOKEN_SYMBOL_MINUS_EQUAL,
			TokenType::TOKEN_SYMBOL_STAR_EQUAL,
			TokenType::TOKEN_SYMBOL_SLASH_EQUAL,
			TokenType::TOKEN_SYMBOL_PERCENT_EQUAL,

			// Arithmetic operators
			TokenType::TOKEN_SYMBOL_PLUS,
			TokenType::TOKEN_SYMBOL_MINUS,
			TokenType::TOKEN_SYMBOL_STAR,
			TokenType::TOKEN_SYMBOL_SLASH,
			TokenType::TOKEN_SYMBOL_PERCENT,

			// Logical
			TokenType::TOKEN_SYMBOL_AMPERSAND_AMPERSAND,
			TokenType::TOKEN_SYMBOL_PIPE_PIPE,
		};
		auto binary_operator = Require(k_binary_operators);
		if (!binary_operator) {
			return CreateError(Diagnostic::ParserInternalBinaryOperatorMissing(CurrentTokenOrDefault().data));
		}

		// <expression>
		auto precedence = PrecedenceRule(binary_operator->type).precedence;
		auto rhs        = ParseExpression(precedence);

		return BinaryNode::create(std::move(lhs), std::move(rhs), ASTNode::as_operator(binary_operator->type));
	}

	ASTNode::Dependency Parser::ParseCast()
	{
		// <cast_expression> ::= <keyword_cast> '<' <type_specifier> '>' '(' <expression> ')'

		// <keyword_cast>
		if (!Require(TokenType::TOKEN_KEYWORD_CAST)) {
			return CreateError(
				Diagnostic::ParserUnexpectedKeyword(TokenType::TOKEN_KEYWORD_CAST, CurrentTokenOrDefault().data));
		}

		// '<'
		if (!Require(TokenType::TOKEN_SYMBOL_LESS)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_LESS),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <type_specifier>
		auto type_specifier = ParseTypeSpecifier();
		if (!type_specifier) {
			return CreateError("expected type specifier");
		}

		// '>'
		if (!Require(TokenType::TOKEN_SYMBOL_GREATER)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_GREATER),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// '('
		if (!Require(TokenType::TOKEN_SYMBOL_PAREN_LEFT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_PAREN_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <expression>
		auto expression = ParseExpression();

		// ')'
		if (!Require(TokenType::TOKEN_SYMBOL_PAREN_RIGHT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_PAREN_RIGHT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		return CastNode::create(std::move(expression), std::move(*type_specifier));
	}

	ASTNode::Dependency Parser::ParseForLoop()
	{
		// <for_loop> ::= <keyword_for> '(' [<expression>] ';' [ <expression> ] ';' [ <expression> ] ')'
		// <block_statement>

		// <keyword_for>
		if (!Require(TokenType::TOKEN_KEYWORD_FOR)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(TokenType::TOKEN_KEYWORD_FOR),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// '('
		if (!Require(TokenType::TOKEN_SYMBOL_PAREN_LEFT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_PAREN_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// [Optional] <expression>
		ASTNode::Dependency initialization = nullptr;
		if (!Match(TokenType::TOKEN_SYMBOL_SEMICOLON)) {
			initialization = ParseParameterDeclaration();

			// ';'
			if (!Require(TokenType::TOKEN_SYMBOL_SEMICOLON)) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(TokenType::TOKEN_SYMBOL_SEMICOLON),
				                               std::string(CurrentTokenOrDefault().data)));
			}
		}

		// [Optional] <expression>
		ASTNode::Dependency condition = nullptr;
		if (!Match(TokenType::TOKEN_SYMBOL_SEMICOLON)) {
			condition = ParseExpression();

			// ';'
			if (!Require(TokenType::TOKEN_SYMBOL_SEMICOLON)) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(TokenType::TOKEN_SYMBOL_SEMICOLON),
				                               std::string(CurrentTokenOrDefault().data)));
			}
		}

		// [Optional] <expression>
		ASTNode::Dependency update = nullptr;
		if (!Match(TokenType::TOKEN_SYMBOL_PAREN_RIGHT)) {
			update = ParseExpression();

			// ')'
			if (!Require(TokenType::TOKEN_SYMBOL_PAREN_RIGHT)) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(TokenType::TOKEN_SYMBOL_PAREN_RIGHT),
				                               std::string(CurrentTokenOrDefault().data)));
			}
		}

		// <block_statement>
		auto statements = ParseBlockStatement();

		return ForLoopNode::create(std::move(initialization),
		                           std::move(condition),
		                           std::move(update),
		                           BlockNode::create(std::move(statements)));
	}

	ASTNode::Dependency Parser::ParseFunctionCall(ASTNode::Dependency dependency)
	{
		// <function_call> ::= <identifier> [ '(' <parameter_declaration>, ... ')' ]

		// <identifier>
		if (!dependency->is<LiteralNode>()) {
			const auto previous_token = Peek(-1);
			return CreateError(std::format("expected function name identifier, but got: '{}'",
			                               std::string(previous_token ? previous_token->data : "__ERROR__")));
		}

		// [Optional] '(' <parameter_list> ')'
		ASTNode::Dependencies parameters{};
		if (!Require(TokenType::TOKEN_SYMBOL_PAREN_LEFT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_PAREN_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		const bool parenthesis_next = CurrentTokenOrDefault().type == TokenType::TOKEN_SYMBOL_PAREN_RIGHT;
		if (!parenthesis_next) {
			do {
				// <expression>
				parameters.emplace_back(ParseExpression());
			} while (Match(TokenType::TOKEN_SYMBOL_COMMA));
		}

		if (!Require(TokenType::TOKEN_SYMBOL_PAREN_RIGHT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_PAREN_RIGHT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		return FunctionCallNode::create(std::string(dependency->as<LiteralNode>().value.as<Identifier>()),
		                                std::move(parameters));
	}

	ASTNode::Dependency Parser::ParseFunctionDeclaration()
	{
		// <function_declaration> ::= <keyword_fn> <identifier> [ <parameter_declaration>, ... ] '::' <type_specifier>
		// <block_statement>

		// <keyword_fn>
		if (!Require(TokenType::TOKEN_KEYWORD_FN)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(TokenType::TOKEN_KEYWORD_FN),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <identifier>
		auto name_identifier = Require(TokenType::TOKEN_LITERAL_IDENTIFIER);
		if (!name_identifier) {
			return CreateError(
				std::format("expected function identifier, but got: '{}'", std::string(CurrentTokenOrDefault().data)));
		}

		// [Optional] '(' <parameter_list> ')'
		ASTNode::Dependencies parameters{};
		if (Match(TokenType::TOKEN_SYMBOL_PAREN_LEFT)) {
			const bool parenthesis_next = CurrentTokenOrDefault().type == TokenType::TOKEN_SYMBOL_PAREN_RIGHT;
			if (!parenthesis_next) {
				do {
					parameters.emplace_back(ParseParameterDeclaration());
				} while (Match(TokenType::TOKEN_SYMBOL_COMMA));
			}

			if (!Require(TokenType::TOKEN_SYMBOL_PAREN_RIGHT)) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(TokenType::TOKEN_SYMBOL_PAREN_RIGHT),
				                               std::string(CurrentTokenOrDefault().data)));
			}
		}

		// '::'
		if (!Require(TokenType::TOKEN_SYMBOL_COLON_COLON)) {
			return CreateError(std::format("expected type separator '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_COLON_COLON),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <identifier>
		auto type_specifier = ParseTypeSpecifier();
		if (!type_specifier) {
			return CreateError("expected type specifier");
		}

		auto statements = ParseBlockStatement();

		return FunctionDeclarationNode::create(std::string(name_identifier->data),
		                                       std::move(*type_specifier),
		                                       std::move(parameters),
		                                       BlockNode::create(std::move(statements)));
	}

	ASTNode::Dependency Parser::ParseGrouping()
	{
		// <grouping_expression> ::= '(' <expression> ')'

		// '('
		if (!Require(TokenType::TOKEN_SYMBOL_PAREN_LEFT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_PAREN_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <expression>
		auto expression = ParseExpression();

		// ')'
		if (!Require(TokenType::TOKEN_SYMBOL_PAREN_RIGHT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_PAREN_RIGHT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		return expression;
	}

	ASTNode::Dependency Parser::ParseIf()
	{
		// <if_statement> ::= <keyword_if> '(' <expression> ')' <block_statement> [ <keyword_else> <block_statement> ]

		// <keyword_if>
		if (!Require(TokenType::TOKEN_KEYWORD_IF)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(TokenType::TOKEN_KEYWORD_IF),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// '('
		if (!Require(TokenType::TOKEN_SYMBOL_PAREN_LEFT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_PAREN_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <expression>
		auto condition = ParseExpression();

		// ')'
		if (!Require(TokenType::TOKEN_SYMBOL_PAREN_RIGHT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_PAREN_RIGHT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <block_statement>
		auto true_statements = ParseBlockStatement();

		// [ <keyword_else> <block_statement> ]
		ASTNode::Dependencies false_statements{};
		if (Match(TokenType::TOKEN_KEYWORD_ELSE)) {
			false_statements = ParseBlockStatement();
		}

		return IfNode::create(std::move(condition),
		                      BlockNode::create(std::move(true_statements)),
		                      BlockNode::create(std::move(false_statements)));
	}

	ASTNode::Dependency Parser::ParseLiteral()
	{
		// <literal> ::= <integer_literal>  | <float_literal> | <string_literal> | <keyword_true> | <keyword_false>

		const auto& token = Require(std::array{ TokenType::TOKEN_KEYWORD_FALSE,
		                                        TokenType::TOKEN_KEYWORD_NULL,
		                                        TokenType::TOKEN_KEYWORD_TRUE,
		                                        TokenType::TOKEN_LITERAL_FLOAT,
		                                        TokenType::TOKEN_LITERAL_IDENTIFIER,
		                                        TokenType::TOKEN_LITERAL_INTEGER,
		                                        TokenType::TOKEN_LITERAL_STRING });
		if (!token) {
			return CreateError(
				std::format("expected literal expression, but got: '{}'", Token::Name(CurrentTokenOrDefault().type)));
		}

		if (token->type == TokenType::TOKEN_LITERAL_FLOAT) {
			auto result = ParseIntegralValue<f64>(token->data);
			if (!result.has_value()) {
				return CreateError(std::format("failed to parse float expression, because: '{}'", result.error()));
			}
			if (*result <= std::numeric_limits<f32>::lowest() || *result >= std::numeric_limits<f32>::max()) {
				return LiteralNode::create(Scalar::create<PrimitiveType::Kind::Float64>(*result));
			}
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::Float32>(*result));
		}

		if (token->type == TokenType::TOKEN_LITERAL_INTEGER) {
			auto result = ParseIntegralValue<i64>(token->data);
			if (!result.has_value()) {
				return CreateError(std::format("failed to parse integer expression, because: '{}'", result.error()));
			}
			if (*result <= std::numeric_limits<i32>::lowest() || *result >= std::numeric_limits<i32>::max()) {
				return LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int64>(*result));
			}
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(*result));
		}

		if (token->type == TokenType::TOKEN_LITERAL_STRING) {
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::String>(token->data));
		}

		if (token->type == TokenType::TOKEN_LITERAL_IDENTIFIER) {
			return LiteralNode::create(Identifier::create(token->data));
		}

		if (token->type == TokenType::TOKEN_KEYWORD_TRUE) {
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::Boolean>(true));
		}

		if (token->type == TokenType::TOKEN_KEYWORD_FALSE) {
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::Boolean>(false));
		}

		if (token->type == TokenType::TOKEN_KEYWORD_NULL) {
			return LiteralNode::create({});
		}

		return ErrorNode::create("[INTERNAL] unknown literal");
	}

	ASTNode::Dependency Parser::ParseLoopControl()
	{  // <loop_control> ::= <keyword_break> | <keyword_continue>

		auto token = Require(std::array{ TokenType::TOKEN_KEYWORD_BREAK, TokenType::TOKEN_KEYWORD_CONTINUE });
		if (!token) {
			return CreateError(std::format("expected '{}' or '{}' keyword, but got: '{}'",
			                               Token::Name(TokenType::TOKEN_KEYWORD_BREAK),
			                               Token::Name(TokenType::TOKEN_KEYWORD_CONTINUE),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <keyword_break> | <keyword_continue>
		const auto control_type = token->type == TokenType::TOKEN_KEYWORD_BREAK ? LoopControlNode::Type::Break
		                                                                        : LoopControlNode::Type::Continue;

		return LoopControlNode::create(control_type);
	}

	ASTNode::Dependency Parser::ParseReturn()
	{
		// <return_statement> ::= <keyword_return> [ <expression> ]

		// <keyword_return>
		if (!Require(TokenType::TOKEN_KEYWORD_RETURN)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(TokenType::TOKEN_KEYWORD_RETURN),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// [ <expression> ]
		ASTNode::Dependency expression = nullptr;
		if (CurrentTokenOrDefault().type != TokenType::TOKEN_SYMBOL_SEMICOLON) {
			expression = ParseExpression();
		}

		return ReturnNode::create(std::move(expression));
	}

	ASTNode::Dependency Parser::ParseStructDeclaration()
	{
		// <struct_declaration> ::= <keyword_struct> <identifier> '{' <parameter_declaration> [',' ... ] '}'

		// <keyword_struct>
		if (!Require(TokenType::TOKEN_KEYWORD_STRUCT)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(TokenType::TOKEN_KEYWORD_STRUCT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <identifier>
		auto name_identifier = Require(TokenType::TOKEN_LITERAL_IDENTIFIER);
		if (!name_identifier) {
			return CreateError(
				std::format("expected struct identifier, but got: '{}'", std::string(CurrentTokenOrDefault().data)));
		}

		// '{'
		if (!Require(TokenType::TOKEN_SYMBOL_BRACE_LEFT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_BRACE_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		ASTNode::Dependencies parameters{};
		if (const auto current_type = CurrentTokenOrDefault().type;
		    current_type != TokenType::TOKEN_SYMBOL_BRACE_RIGHT) {
			if (current_type == TokenType::TOKEN_SPECIAL_END_OF_FILE) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(TokenType::TOKEN_SYMBOL_BRACE_RIGHT),
				                               std::string(CurrentTokenOrDefault().data)));
			}

			while (!Match(TokenType::TOKEN_SYMBOL_BRACE_RIGHT)) {
				parameters.emplace_back(ParseParameterDeclaration());

				// ','
				if (CurrentTokenOrDefault().type != TokenType::TOKEN_SYMBOL_BRACE_RIGHT
				    && !Require(TokenType::TOKEN_SYMBOL_COMMA)) {
					return CreateError(std::format("expected '{}', but got: '{}'",
					                               Token::Name(TokenType::TOKEN_SYMBOL_COMMA),
					                               std::string(CurrentTokenOrDefault().data)));
				}
			}

			// '}'
			const auto previous_token = Peek(-1);
			if (!previous_token || previous_token->type != TokenType::TOKEN_SYMBOL_BRACE_RIGHT) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(TokenType::TOKEN_SYMBOL_BRACE_RIGHT),
				                               std::string(CurrentTokenOrDefault().data)));
			}
		} else {
			// '}'
			if (!Require(TokenType::TOKEN_SYMBOL_BRACE_RIGHT)) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(TokenType::TOKEN_SYMBOL_BRACE_RIGHT),
				                               std::string(CurrentTokenOrDefault().data)));
			}
		};

		return StructDeclarationNode::create(std::string(name_identifier->data), std::move(parameters));
	}

	ASTNode::Dependency Parser::ParseUnary()
	{
		// <unary_expression> ::= <unary_operator> <expression>

		// <unary_operator>
		static constexpr std::array k_unary_operators = {
			TokenType::TOKEN_SYMBOL_BANG,
			TokenType::TOKEN_SYMBOL_MINUS,
			TokenType::TOKEN_SYMBOL_MINUS_MINUS,
			TokenType::TOKEN_SYMBOL_PLUS_PLUS,
		};
		auto unary_operator = Require(k_unary_operators);
		if (!unary_operator) {
			return CreateError(
				std::format("expected unary operator, but got: '{}'", std::string(CurrentTokenOrDefault().data)));
		}

		// <expression>
		auto expression = ParseExpression(Precedence::PRECEDENCE_PREFIX);

		return UnaryNode::create(std::move(expression), ASTNode::as_operator(unary_operator->type));
	}

	ASTNode::Dependency Parser::ParseVariableDeclaration()
	{
		// <variable_declaration> ::= <keyword_let> [ <keyword_mut> ] <identifier> ':' <type_specifier> '=' <expression>

		// <keyword_let>
		if (!Require(TokenType::TOKEN_KEYWORD_LET)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(TokenType::TOKEN_KEYWORD_LET),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// [Optional] <keyword_mut>
		const bool is_mutable = Match(TokenType::TOKEN_KEYWORD_MUT);

		// <identifier>
		auto name_identifier = Require(TokenType::TOKEN_LITERAL_IDENTIFIER);
		if (!name_identifier) {
			return CreateError(
				std::format("expected variable identifier, but got: '{}'", std::string(CurrentTokenOrDefault().data)));
		}

		// ':'
		if (!Require(TokenType::TOKEN_SYMBOL_COLON)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_COLON),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <type_specifier>
		auto type_specifier = ParseTypeSpecifier();
		if (!type_specifier) {
			return CreateError("expected type specifier");
		}

		// '='
		if (!Require(TokenType::TOKEN_SYMBOL_EQUAL)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_EQUAL),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <expression>
		auto expression = ParseExpression();

		return VariableDeclarationNode::create(
			std::string(name_identifier->data), std::move(*type_specifier), std::move(expression), is_mutable);
	}

	ASTNode::Dependency Parser::ParseWhileLoop()
	{
		// <while_loop> ::= <keyword_while> [ '(' <expression> ')' ] <block_statement>

		// <keyword_while>
		if (!Require(TokenType::TOKEN_KEYWORD_WHILE)) {
			return CreateError(std::format("expected '{}' keyword, but got: '{}'",
			                               Token::Name(TokenType::TOKEN_KEYWORD_WHILE),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// [Optional] '(' <expression> ')'
		ASTNode::Dependency condition{};
		if (Match(TokenType::TOKEN_SYMBOL_PAREN_LEFT)) {
			// <expression>
			condition = ParseExpression();

			// ')'
			if (!Require(TokenType::TOKEN_SYMBOL_PAREN_RIGHT)) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(TokenType::TOKEN_SYMBOL_PAREN_RIGHT),
				                               std::string(CurrentTokenOrDefault().data)));
			}
		} else {
			condition = LiteralNode::create(Scalar::create<PrimitiveType::Kind::Boolean>(true));
		}

		// <block_statement>
		auto statements = ParseBlockStatement();

		return WhileNode::create(std::move(condition), BlockNode::create(std::move(statements)));
	}

	ASTNode::Dependencies Parser::ParseBlockStatement()
	{
		// <block_statement> ::= '{' [ <statement> ';' ... ] '}'

		ASTNode::Dependencies statements{};
		if (!Require(TokenType::TOKEN_SYMBOL_BRACE_LEFT)) {
			statements.emplace_back(CreateError(std::format("expected '{}', but got: '{}'",
			                                                Token::Name(TokenType::TOKEN_SYMBOL_BRACE_LEFT),
			                                                std::string(CurrentTokenOrDefault().data))));
			return statements;
		}

		while (!Match(TokenType::TOKEN_SYMBOL_BRACE_RIGHT)) {
			if (CurrentTokenOrDefault().type == TokenType::TOKEN_SPECIAL_END_OF_FILE) {
				break;
			}

			// NOTE: We might be looking at a stray semicolon(s). Consume until nothing is left.
			while (Match(TokenType::TOKEN_SYMBOL_SEMICOLON)) {
				// ...
			}
			if (Match(TokenType::TOKEN_SYMBOL_BRACE_RIGHT)) {
				break;
			}

			statements.emplace_back(ParseStatement());

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

			if (must_consume_semicolon && CurrentTokenOrDefault().type != TokenType::TOKEN_SYMBOL_BRACE_RIGHT) {
				// ';'
				if (!Require(TokenType::TOKEN_SYMBOL_SEMICOLON)) {
					statements.emplace_back(CreateError(std::format("expected '{}', but got: '{}'",
					                                                Token::Name(TokenType::TOKEN_SYMBOL_SEMICOLON),
					                                                std::string(CurrentTokenOrDefault().data))));
					return statements;
				}
			}
		}

		const auto previous_token = Peek(-1);
		if (!previous_token || previous_token->type != TokenType::TOKEN_SYMBOL_BRACE_RIGHT) {
			statements.emplace_back(CreateError(std::format("expected '{}', but got: '{}'",
			                                                Token::Name(TokenType::TOKEN_SYMBOL_BRACE_RIGHT),
			                                                std::string(CurrentTokenOrDefault().data))));
			return statements;
		}

		return statements;
	}

	ASTNode::Dependency Parser::ParseInitializerList()
	{
		// <initializer_list> ::= '{' [ <expression> [ ',' ...] ] '}'
		static constexpr auto k_delimiter = TokenType::TOKEN_SYMBOL_COMMA;

		if (!Require(TokenType::TOKEN_SYMBOL_BRACE_LEFT)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_BRACE_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		ASTNode::Dependencies expressions{};
		while (!Match(TokenType::TOKEN_SYMBOL_BRACE_RIGHT)) {
			if (CurrentTokenOrDefault().type == TokenType::TOKEN_SPECIAL_END_OF_FILE) {
				return CreateError(std::format("expected '{}', but got: '{}'",
				                               Token::Name(TokenType::TOKEN_SYMBOL_BRACE_RIGHT),
				                               std::string(CurrentTokenOrDefault().data)));
			}

			expressions.emplace_back(ParseExpression());

			// We need to consume the ',' symbol only if the next statement is not a right brace...
			if (CurrentTokenOrDefault().type != TokenType::TOKEN_SYMBOL_BRACE_RIGHT) {
				if (!Require(k_delimiter)) {
					return CreateError(std::format("expected '{}', but got: '{}'",
					                               Token::Name(k_delimiter),
					                               std::string(CurrentTokenOrDefault().data)));
				}
			}
		}

		return BlockNode::create(std::move(expressions));
	}

	ASTNode::Dependency Parser::ParseParameterDeclaration()
	{
		// <parameter_declaration> ::= <identifier> ':' <type_specifier> [ '=' <expression> ]

		// <identifier>
		auto name_identifier = Require(TokenType::TOKEN_LITERAL_IDENTIFIER);
		if (!name_identifier) {
			return CreateError(
				std::format("expected name identifier, but got: '{}'", std::string(CurrentTokenOrDefault().data)));
		}

		// ':'
		if (!Require(TokenType::TOKEN_SYMBOL_COLON)) {
			return CreateError(std::format("expected '{}', but got: '{}'",
			                               Token::Name(TokenType::TOKEN_SYMBOL_BRACE_LEFT),
			                               std::string(CurrentTokenOrDefault().data)));
		}

		// <type_specifier>
		auto type_specifier = ParseTypeSpecifier();
		if (!type_specifier) {
			return CreateError("expected type specifier");
		}

		// [ '=' <expression> ]
		ASTNode::Dependency expression = nullptr;
		if (Match(TokenType::TOKEN_SYMBOL_EQUAL)) {
			expression = ParseExpression();
		}

		return VariableDeclarationNode::create(
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
		if (current_token->type == TokenType::TOKEN_LITERAL_IDENTIFIER) {
			Require(TokenType::TOKEN_LITERAL_IDENTIFIER);
			return TypeSpecifier(BaseTypeSpecifier(std::string(current_token->data)));
		}

		// <array_type_specifier> ::=  '[' <type_specifier> [ ',' <integer_literal> ] ']'
		if (current_token->type == TokenType::TOKEN_SYMBOL_BRACKET_LEFT) {
			// '['
			if (!Require(TokenType::TOKEN_SYMBOL_BRACKET_LEFT)) {
				return std::nullopt;
			}
			auto type_specifier = ParseTypeSpecifier();
			if (!type_specifier) {
				return std::nullopt;
			}

			// [Optional] ','
			ArrayTypeSpecifier::Size array_size = ArrayTypeSpecifier::k_unbound_size;
			if (Match(TokenType::TOKEN_SYMBOL_COMMA)) {
				// <integer_literal>
				const auto integer_literal = Require(TokenType::TOKEN_LITERAL_INTEGER);
				if (!integer_literal) {
					return std::nullopt;
				}

				auto array_size_literal_result = ParseIntegralValue<i64>(integer_literal->data);
				if (!array_size_literal_result.has_value()) {
					return std::nullopt;
				}
				array_size = array_size_literal_result.value();
			}

			// ']'
			if (!Require(TokenType::TOKEN_SYMBOL_BRACKET_RIGHT)) {
				return std::nullopt;
			}
			return TypeSpecifier(ArrayTypeSpecifier(std::move(*type_specifier), array_size));
		}

		// <pointer_type_specifier> ::= '*' <type_specifier>
		if (current_token->type == TokenType::TOKEN_SYMBOL_STAR) {
			Require(TokenType::TOKEN_SYMBOL_STAR);
			auto type_specifier = ParseTypeSpecifier();
			if (!type_specifier) {
				return std::nullopt;
			}
			return TypeSpecifier(PointerTypeSpecifier(std::move(*type_specifier)));
		}

		// [INTERNAL]: invalid type specifier
		return std::nullopt;
	}

	ASTNode::Dependency Parser::CreateError(Diagnostic diagnostic)
	{
		static constexpr std::array k_synchronization_tokens = {
			TokenType::TOKEN_KEYWORD_ELSE,       TokenType::TOKEN_KEYWORD_FN,         TokenType::TOKEN_KEYWORD_FOR,
			TokenType::TOKEN_KEYWORD_IF,         TokenType::TOKEN_KEYWORD_LET,        TokenType::TOKEN_KEYWORD_NATIVE,
			TokenType::TOKEN_KEYWORD_STRUCT,     TokenType::TOKEN_KEYWORD_WHILE,      TokenType::TOKEN_SYMBOL_SEMICOLON,
			TokenType::TOKEN_SYMBOL_BRACE_RIGHT, TokenType::TOKEN_SYMBOL_PAREN_RIGHT,
		};
		while (_current_token != std::end(_tokens)) {
			if (std::ranges::contains(k_synchronization_tokens, _current_token->type)) {
				++_current_token;
				break;  // Synchronized.
			}
			++_current_token;
		}

		return ErrorNode::create(std::move(error_message));
	}

	std::optional<Token> Parser::Require(TokenType type)
	{
		if (_current_token == std::end(_tokens) || _current_token->type != type) {
			return std::nullopt;
		}
		return *_current_token++;
	}

	std::optional<Token> Parser::Require(std::span<const TokenType> types)
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

	bool Parser::Match(TokenType type)
	{
		if (_current_token == std::end(_tokens) || _current_token->type != type) {
			return false;
		}
		++_current_token;
		return true;
	}

	Parser::PrecedenceRule Parser::PrecedenceRule(TokenType type) noexcept
	{
		switch (type) {
			// Assignment
			case TokenType::TOKEN_SYMBOL_EQUAL:
			case TokenType::TOKEN_SYMBOL_PLUS_EQUAL:
			case TokenType::TOKEN_SYMBOL_MINUS_EQUAL:
			case TokenType::TOKEN_SYMBOL_STAR_EQUAL:
			case TokenType::TOKEN_SYMBOL_SLASH_EQUAL:
			case TokenType::TOKEN_SYMBOL_PERCENT_EQUAL:
				return { Precedence::PRECEDENCE_ASSIGN, nullptr, &Parser::ParseBinary };

			// Comparison
			case TokenType::TOKEN_SYMBOL_LESS:
			case TokenType::TOKEN_SYMBOL_LESS_EQUAL:
			case TokenType::TOKEN_SYMBOL_GREATER:
			case TokenType::TOKEN_SYMBOL_GREATER_EQUAL:
				return { Precedence::PRECEDENCE_COMPARE, nullptr, &Parser::ParseBinary };

			// Logical
			case TokenType::TOKEN_SYMBOL_BANG:
				return { Parser::Precedence::PRECEDENCE_PREFIX, &Parser::ParseUnary, nullptr };

			// Literals
			case TokenType::TOKEN_LITERAL_FLOAT:
			case TokenType::TOKEN_LITERAL_IDENTIFIER:
			case TokenType::TOKEN_LITERAL_INTEGER:
			case TokenType::TOKEN_LITERAL_STRING:
			case TokenType::TOKEN_KEYWORD_TRUE:
			case TokenType::TOKEN_KEYWORD_FALSE:
			case TokenType::TOKEN_KEYWORD_NULL:
				return { Precedence::PRECEDENCE_NONE, &Parser::ParseLiteral, nullptr };

			// Arithmetic
			case TokenType::TOKEN_SYMBOL_MINUS:
				return { Precedence::PRECEDENCE_ADDITIVE, &Parser::ParseUnary, &Parser::ParseBinary };
			case TokenType::TOKEN_SYMBOL_PLUS:
				return { Precedence::PRECEDENCE_ADDITIVE, nullptr, &Parser::ParseBinary };
			case TokenType::TOKEN_SYMBOL_PERCENT:
			case TokenType::TOKEN_SYMBOL_SLASH:
			case TokenType::TOKEN_SYMBOL_STAR:
				return { Precedence::PRECEDENCE_MULTIPLICATIVE, nullptr, &Parser::ParseBinary };
			case TokenType::TOKEN_SYMBOL_PLUS_PLUS:
			case TokenType::TOKEN_SYMBOL_MINUS_MINUS:
				return { Precedence::PRECEDENCE_PREFIX, &Parser::ParseUnary, nullptr };

			// Other
			case TokenType::TOKEN_KEYWORD_CAST:
				return { Precedence::PRECEDENCE_NONE, &Parser::ParseCast, nullptr };
			case TokenType::TOKEN_SYMBOL_PAREN_LEFT:
				return { Precedence::PRECEDENCE_POSTFIX, &Parser::ParseGrouping, &Parser::ParseFunctionCall };
			case TokenType::TOKEN_SYMBOL_BRACE_LEFT:
				return { Precedence::PRECEDENCE_NONE, &Parser::ParseInitializerList, nullptr };
			default:
				break;
		}

		return { Precedence::PRECEDENCE_NONE, nullptr, nullptr };  // No precedence.
	}

	Token Parser::CurrentTokenOrDefault() const noexcept
	{
		if (_current_token == std::end(_tokens)) {
			auto last_token = _tokens.back();
			return Token{
				.type     = TokenType::TOKEN_SPECIAL_END_OF_FILE,
				.data     = Token::NameInternal(TokenType::TOKEN_SPECIAL_END_OF_FILE),
				.location = SourceOffset{ last_token.location.row,
                                         static_cast<u32>(last_token.location.column + last_token.data.size()) }
			};
		}
		return *_current_token;
	}

}  // namespace soul::parser
