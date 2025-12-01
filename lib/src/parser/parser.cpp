#include "parser/parser.h"

#include "ast/ast.h"

#include <algorithm>
#include <array>
#include <format>

namespace soul::parser
{
	using namespace soul::ast;
	using namespace soul::types;

	/** @brief Operator precedence (LOWEST to HIGHEST) */
	enum class Parser::Precedence : u8
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

	ast::ASTNode::Dependency Parser::parse(std::string_view module_name, std::span<const Token> tokens)
	{
		return Parser{ module_name, tokens }.parse();
	}

	ast::ASTNode::Dependency Parser::parse()
	{
		if (_tokens.empty()) {
			return ModuleNode::create(std::string(_module_name), {});
		}

		ASTNode::Dependencies statements{};
		for (const auto& token : _tokens) {
			if (token.type == Token::Type::SpecialError) {
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
			case Token::Type::KeywordBreak:
			case Token::Type::KeywordContinue:
				return parse_loop_control();
			case Token::Type::KeywordFn:
				return parse_function_declaration();
			case Token::Type::KeywordFor:
				return parse_for_loop();
			case Token::Type::KeywordIf:
				return parse_if();
			case Token::Type::KeywordLet:
				return parse_variable_declaration();
			case Token::Type::KeywordReturn:
				return parse_return();
			case Token::Type::KeywordStruct:
				return parse_struct_declaration();
			case Token::Type::KeywordWhile:
				return parse_while_loop();
			case Token::Type::SymbolBraceLeft:
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
			                                Token::internal_name(current_token_or_default().type)));
		}

		auto prefix_expression = (this->*prefix_rule)();

		while (precedence <= precedence_rule(current_token_or_default().type).precedence) {
			auto infix_rule = precedence_rule(current_token_or_default().type).infix;
			if (!infix_rule) [[unlikely]] {
				return create_error(std::format("[INTERNAL] no infix precedence rule for '{}' was specified.",
				                                Token::internal_name(current_token_or_default().type)));
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
			Token::Type::SymbolGreater,
			Token::Type::SymbolGreaterEqual,
			Token::Type::SymbolLess,
			Token::Type::SymbolLessEqual,
			Token::Type::SymbolEqualEqual,
			Token::Type::SymbolBangEqual,

			// Assignment
			Token::Type::SymbolEqual,
			Token::Type::SymbolPlusEqual,
			Token::Type::SymbolMinusEqual,
			Token::Type::SymbolStarEqual,
			Token::Type::SymbolSlashEqual,
			Token::Type::SymbolPercentEqual,

			// Arithmetic operators
			Token::Type::SymbolPlus,
			Token::Type::SymbolMinus,
			Token::Type::SymbolStar,
			Token::Type::SymbolSlash,
			Token::Type::SymbolPercent,

			// Logical
			Token::Type::SymbolAmpersandAmpersand,
			Token::Type::SymbolPipePipe,
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
		if (!require(Token::Type::KeywordCast)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::name(Token::Type::KeywordCast),
			                                std::string(current_token_or_default().data)));
		}

		// '<'
		if (!require(Token::Type::SymbolLess)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolLess),
			                                std::string(current_token_or_default().data)));
		}

		// <type_specifier>
		auto type_specifier = parse_type_specifier();
		if (!type_specifier) {
			return create_error("expected type specifier");
		}

		// '>'
		if (!require(Token::Type::SymbolGreater)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolGreater),
			                                std::string(current_token_or_default().data)));
		}

		// '('
		if (!require(Token::Type::SymbolParenLeft)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolParenLeft),
			                                std::string(current_token_or_default().data)));
		}

		// <expression>
		auto expression = parse_expression();

		// ')'
		if (!require(Token::Type::SymbolParenRight)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolParenRight),
			                                std::string(current_token_or_default().data)));
		}

		return CastNode::create(std::move(expression), std::move(*type_specifier));
	}

	ASTNode::Dependency Parser::parse_for_loop()
	{
		// <for_loop> ::= <keyword_for> '(' [<expression>] ';' [ <expression> ] ';' [ <expression> ] ')'
		// <block_statement>

		// <keyword_for>
		if (!require(Token::Type::KeywordFor)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::name(Token::Type::KeywordFor),
			                                std::string(current_token_or_default().data)));
		}

		// '('
		if (!require(Token::Type::SymbolParenLeft)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolParenLeft),
			                                std::string(current_token_or_default().data)));
		}

		// [Optional] <expression>
		ASTNode::Dependency initialization = nullptr;
		if (!match(Token::Type::SymbolSemicolon)) {
			initialization = parse_parameter_declaration();

			// ';'
			if (!require(Token::Type::SymbolSemicolon)) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::name(Token::Type::SymbolSemicolon),
				                                std::string(current_token_or_default().data)));
			}
		}

		// [Optional] <expression>
		ASTNode::Dependency condition = nullptr;
		if (!match(Token::Type::SymbolSemicolon)) {
			condition = parse_expression();

			// ';'
			if (!require(Token::Type::SymbolSemicolon)) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::name(Token::Type::SymbolSemicolon),
				                                std::string(current_token_or_default().data)));
			}
		}

		// [Optional] <expression>
		ASTNode::Dependency update = nullptr;
		if (!match(Token::Type::SymbolParenRight)) {
			update = parse_expression();

			// ')'
			if (!require(Token::Type::SymbolParenRight)) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::name(Token::Type::SymbolParenRight),
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
		if (!require(Token::Type::SymbolParenLeft)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolParenLeft),
			                                std::string(current_token_or_default().data)));
		}

		const bool parenthesis_next = current_token_or_default().type == Token::Type::SymbolParenRight;
		if (!parenthesis_next) {
			do {
				// <expression>
				parameters.emplace_back(parse_expression());
			} while (match(Token::Type::SymbolComma));
		}

		if (!require(Token::Type::SymbolParenRight)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolParenRight),
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
		if (!require(Token::Type::KeywordFn)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::name(Token::Type::KeywordFn),
			                                std::string(current_token_or_default().data)));
		}

		// <identifier>
		auto name_identifier = require(Token::Type::LiteralIdentifier);
		if (!name_identifier) {
			return create_error(std::format("expected function identifier, but got: '{}'",
			                                std::string(current_token_or_default().data)));
		}

		// [Optional] '(' <parameter_list> ')'
		ASTNode::Dependencies parameters{};
		if (match(Token::Type::SymbolParenLeft)) {
			const bool parenthesis_next = current_token_or_default().type == Token::Type::SymbolParenRight;
			if (!parenthesis_next) {
				do {
					parameters.emplace_back(parse_parameter_declaration());
				} while (match(Token::Type::SymbolComma));
			}

			if (!require(Token::Type::SymbolParenRight)) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::name(Token::Type::SymbolParenRight),
				                                std::string(current_token_or_default().data)));
			}
		}

		// '::'
		if (!require(Token::Type::SymbolColonColon)) {
			return create_error(std::format("expected type separator '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolColonColon),
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

	ast::ASTNode::Dependency Parser::parse_grouping()
	{
		// <grouping_expression> ::= '(' <expression> ')'

		// '('
		if (!require(Token::Type::SymbolParenLeft)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolParenLeft),
			                                std::string(current_token_or_default().data)));
		}

		// <expression>
		auto expression = parse_expression();

		// ')'
		if (!require(Token::Type::SymbolParenRight)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolParenRight),
			                                std::string(current_token_or_default().data)));
		}

		return expression;
	}

	ASTNode::Dependency Parser::parse_if()
	{
		// <if_statement> ::= <keyword_if> '(' <expression> ')' <block_statement> [ <keyword_else> <block_statement> ]

		// <keyword_if>
		if (!require(Token::Type::KeywordIf)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::name(Token::Type::KeywordIf),
			                                std::string(current_token_or_default().data)));
		}

		// '('
		if (!require(Token::Type::SymbolParenLeft)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolParenLeft),
			                                std::string(current_token_or_default().data)));
		}

		// <expression>
		auto condition = parse_expression();

		// ')'
		if (!require(Token::Type::SymbolParenRight)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolParenRight),
			                                std::string(current_token_or_default().data)));
		}

		// <block_statement>
		auto true_statements = parse_block_statement();

		// [ <keyword_else> <block_statement> ]
		ASTNode::Dependencies false_statements{};
		if (match(Token::Type::KeywordElse)) {
			false_statements = parse_block_statement();
		}

		return IfNode::create(std::move(condition),
		                      BlockNode::create(std::move(true_statements)),
		                      BlockNode::create(std::move(false_statements)));
	}

	ASTNode::Dependency Parser::parse_literal()
	{
		// <literal> ::= <integer_literal>  | <float_literal> | <string_literal> | <keyword_true> | <keyword_false>

		const auto& token = require(std::array{ Token::Type::KeywordFalse,
		                                        Token::Type::KeywordNull,
		                                        Token::Type::KeywordTrue,
		                                        Token::Type::LiteralFloat,
		                                        Token::Type::LiteralIdentifier,
		                                        Token::Type::LiteralInteger,
		                                        Token::Type::LiteralString });
		if (!token) {
			return create_error(std::format("expected literal expression, but got: '{}'",
			                                Token::name(current_token_or_default().type)));
		}

		if (token->type == Token::Type::LiteralFloat) {
			f64 v{};
			const auto result = std::from_chars(std::begin(token->data), std::end(token->data), v);
			if (!result) {
				return create_error(std::format("failed to parse float expression, because: '{}'",
				                                std::make_error_condition(result.ec).message()));
			}
			if (v <= std::numeric_limits<f32>::lowest() || v >= std::numeric_limits<f32>::max()) {
				return LiteralNode::create(Scalar::create<PrimitiveType::Kind::Float64>(v));
			}
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::Float32>(v));
		}

		if (token->type == Token::Type::LiteralInteger) {
			i64 v{};
			const auto result = std::from_chars(std::begin(token->data), std::end(token->data), v);
			if (!result) {
				return create_error(std::format("failed to parse integer expression, because: '{}'",
				                                std::make_error_condition(result.ec).message()));
			}
			if (v <= std::numeric_limits<i32>::lowest() || v >= std::numeric_limits<i32>::max()) {
				return LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int64>(v));
			}
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(v));
		}

		if (token->type == Token::Type::LiteralString) {
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::String>(token->data));
		}

		if (token->type == Token::Type::LiteralIdentifier) {
			return LiteralNode::create(Identifier::create(token->data));
		}

		if (token->type == Token::Type::KeywordTrue) {
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::Boolean>(true));
		}

		if (token->type == Token::Type::KeywordFalse) {
			return LiteralNode::create(Scalar::create<PrimitiveType::Kind::Boolean>(false));
		}

		if (token->type == Token::Type::KeywordNull) {
			return LiteralNode::create({});
		}

		return ErrorNode::create("[INTERNAL] unknown literal");
	}

	ast::ASTNode::Dependency Parser::parse_loop_control()
	{  // <loop_control> ::= <keyword_break> | <keyword_continue>

		auto token = require(std::array{ Token::Type::KeywordBreak, Token::Type::KeywordContinue });
		if (!token) {
			return create_error(std::format("expected '{}' or '{}' keyword, but got: '{}'",
			                                Token::name(Token::Type::KeywordBreak),
			                                Token::name(Token::Type::KeywordContinue),
			                                std::string(current_token_or_default().data)));
		}

		// <keyword_break> | <keyword_continue>
		const auto control_type
			= token->type == Token::Type::KeywordBreak ? LoopControlNode::Type::Break : LoopControlNode::Type::Continue;

		return LoopControlNode::create(control_type);
	}

	ast::ASTNode::Dependency Parser::parse_return()
	{
		// <return_statement> ::= <keyword_return> [ <expression> ]

		// <keyword_return>
		if (!require(Token::Type::KeywordReturn)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::name(Token::Type::KeywordReturn),
			                                std::string(current_token_or_default().data)));
		}

		// [ <expression> ]
		ASTNode::Dependency expression = nullptr;
		if (current_token_or_default().type != Token::Type::SymbolSemicolon) {
			expression = parse_expression();
		}

		return ReturnNode::create(std::move(expression));
	}

	ASTNode::Dependency Parser::parse_struct_declaration()
	{
		// <struct_declaration> ::= <keyword_struct> <identifier> '{' <parameter_declaration> [',' ... ] '}'

		// <keyword_struct>
		if (!require(Token::Type::KeywordStruct)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::name(Token::Type::KeywordStruct),
			                                std::string(current_token_or_default().data)));
		}

		// <identifier>
		auto name_identifier = require(Token::Type::LiteralIdentifier);
		if (!name_identifier) {
			return create_error(
				std::format("expected struct identifier, but got: '{}'", std::string(current_token_or_default().data)));
		}

		// '{'
		if (!require(Token::Type::SymbolBraceLeft)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolBraceLeft),
			                                std::string(current_token_or_default().data)));
		}

		ASTNode::Dependencies parameters{};
		if (const auto current_type = current_token_or_default().type; current_type != Token::Type::SymbolBraceRight) {
			if (current_type == Token::Type::SpecialEndOfFile) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::name(Token::Type::SymbolBraceRight),
				                                std::string(current_token_or_default().data)));
			}

			while (!match(Token::Type::SymbolBraceRight)) {
				parameters.emplace_back(parse_parameter_declaration());

				// ','
				if (current_token_or_default().type != Token::Type::SymbolBraceRight
				    && !require(Token::Type::SymbolComma)) {
					return create_error(std::format("expected '{}', but got: '{}'",
					                                Token::name(Token::Type::SymbolComma),
					                                std::string(current_token_or_default().data)));
				}
			}

			// '}'
			const auto previous_token = peek(-1);
			if (!previous_token || previous_token->type != Token::Type::SymbolBraceRight) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::name(Token::Type::SymbolBraceRight),
				                                std::string(current_token_or_default().data)));
			}
		} else {
			// '}'
			if (!require(Token::Type::SymbolBraceRight)) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::name(Token::Type::SymbolBraceRight),
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
			Token::Type::SymbolBang,
			Token::Type::SymbolMinus,
			Token::Type::SymbolMinusMinus,
			Token::Type::SymbolPlusPlus,
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
		if (!require(Token::Type::KeywordLet)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::name(Token::Type::KeywordLet),
			                                std::string(current_token_or_default().data)));
		}

		// [Optional] <keyword_mut>
		const bool is_mutable = match(Token::Type::KeywordMut);

		// <identifier>
		auto name_identifier = require(Token::Type::LiteralIdentifier);
		if (!name_identifier) {
			return create_error(std::format("expected variable identifier, but got: '{}'",
			                                std::string(current_token_or_default().data)));
		}

		// ':'
		if (!require(Token::Type::SymbolColon)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolColon),
			                                std::string(current_token_or_default().data)));
		}

		// <type_specifier>
		auto type_specifier = parse_type_specifier();
		if (!type_specifier) {
			return create_error("expected type specifier");
		}

		// '='
		if (!require(Token::Type::SymbolEqual)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolEqual),
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
		if (!require(Token::Type::KeywordWhile)) {
			return create_error(std::format("expected '{}' keyword, but got: '{}'",
			                                Token::name(Token::Type::KeywordWhile),
			                                std::string(current_token_or_default().data)));
		}

		// [Optional] '(' <expression> ')'
		ASTNode::Dependency condition{};
		if (match(Token::Type::SymbolParenLeft)) {
			// <expression>
			condition = parse_expression();

			// ')'
			if (!require(Token::Type::SymbolParenRight)) {
				return create_error(std::format("expected '{}', but got: '{}'",
				                                Token::name(Token::Type::SymbolParenRight),
				                                std::string(current_token_or_default().data)));
			}
		} else {
			condition = LiteralNode::create(Scalar::create<PrimitiveType::Kind::Boolean>(true));
		}

		// <block_statement>
		auto statements = parse_block_statement();

		return WhileNode::create(std::move(condition), BlockNode::create(std::move(statements)));
	}

	ASTNode::Dependencies Parser::parse_block_statement()
	{
		// <block_statement> ::= '{' [ <statement> ';' ... ] '}'

		ASTNode::Dependencies statements{};
		if (!require(Token::Type::SymbolBraceLeft)) {
			statements.emplace_back(create_error(std::format("expected '{}', but got: '{}'",
			                                                 Token::name(Token::Type::SymbolBraceLeft),
			                                                 std::string(current_token_or_default().data))));
			return statements;
		}

		while (!match(Token::Type::SymbolBraceRight)) {
			if (current_token_or_default().type == Token::Type::SpecialEndOfFile) {
				break;
			}

			// NOTE: We might be looking at a stray semicolon(s). Consume until nothing is left.
			while (match(Token::Type::SymbolSemicolon)) {
				// ...
			}
			if (match(Token::Type::SymbolBraceRight)) {
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

			if (must_consume_semicolon && current_token_or_default().type != Token::Type::SymbolBraceRight) {
				// ';'
				if (!require(Token::Type::SymbolSemicolon)) {
					statements.emplace_back(create_error(std::format("expected '{}', but got: '{}'",
					                                                 Token::name(Token::Type::SymbolSemicolon),
					                                                 std::string(current_token_or_default().data))));
					return statements;
				}
			}
		}

		const auto previous_token = peek(-1);
		if (!previous_token || previous_token->type != Token::Type::SymbolBraceRight) {
			statements.emplace_back(create_error(std::format("expected '{}', but got: '{}'",
			                                                 Token::name(Token::Type::SymbolBraceRight),
			                                                 std::string(current_token_or_default().data))));
			return statements;
		}

		return statements;
	}

	ASTNode::Dependency Parser::parse_parameter_declaration()
	{
		// <parameter_declaration> ::= <identifier> ':' <type_specifier> [ '=' <expression> ]

		// <identifier>
		auto name_identifier = require(Token::Type::LiteralIdentifier);
		if (!name_identifier) {
			return create_error(
				std::format("expected name identifier, but got: '{}'", std::string(current_token_or_default().data)));
		}

		// ':'
		if (!require(Token::Type::SymbolColon)) {
			return create_error(std::format("expected '{}', but got: '{}'",
			                                Token::name(Token::Type::SymbolBraceLeft),
			                                std::string(current_token_or_default().data)));
		}

		// <type_specifier>
		auto type_specifier = parse_type_specifier();
		if (!type_specifier) {
			return create_error("expected type specifier");
		}

		// [ '=' <expression> ]
		ASTNode::Dependency expression = nullptr;
		if (match(Token::Type::SymbolEqual)) {
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
		if (current_token->type == Token::Type::LiteralIdentifier) {
			require(Token::Type::LiteralIdentifier);
			return TypeSpecifier(BaseTypeSpecifier(std::string(current_token->data)));
		}

#if 0
		// <array_type_specifier> ::=  '[' <type_specifier> [ ',' <integer_literal> ] ']'
		if (current_token->type == Token::Type::SymbolBracketLeft) {
			require(Token::Type::SymbolBracketLeft);
			auto type_specifier = parse_type_specifier();
			if (!type_specifier) {
				return std::nullopt;
			}
			return TypeSpecifier(ArrayTypeSpecifier(std::move(*type_specifier)));
		}

		// <pointer_type_specifier> ::= '*' <type_specifier>
		if (current_token->type == Token::Type::SymbolStar) {
			require(Token::Type::SymbolStar);
			auto type_specifier = parse_type_specifier();
			if (!type_specifier) {
				return std::nullopt;
			}
			return TypeSpecifier(PointerTypeSpecifier(std::move(*type_specifier)));
		}
#endif

		// [INTERNAL]: invalid type specifier
		return std::nullopt;
	}

	ASTNode::Dependency Parser::create_error(ErrorNode::Message error_message)
	{
		static constexpr std::array k_synchronization_tokens = {
			Token::Type::KeywordElse,      Token::Type::KeywordFn,        Token::Type::KeywordFor,
			Token::Type::KeywordIf,        Token::Type::KeywordLet,       Token::Type::KeywordNative,
			Token::Type::KeywordStruct,    Token::Type::KeywordWhile,     Token::Type::SymbolSemicolon,
			Token::Type::SymbolBraceRight, Token::Type::SymbolParenRight,
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
			case Token::Type::SymbolEqual:
			case Token::Type::SymbolPlusEqual:
			case Token::Type::SymbolMinusEqual:
			case Token::Type::SymbolStarEqual:
			case Token::Type::SymbolSlashEqual:
			case Token::Type::SymbolPercentEqual:
				return { Parser::Precedence::Assign, nullptr, &Parser::parse_binary };

			// Comparison
			case Token::Type::SymbolLess:
			case Token::Type::SymbolLessEqual:
			case Token::Type::SymbolGreater:
			case Token::Type::SymbolGreaterEqual:
				return { Precedence::Compare, nullptr, &Parser::parse_binary };

			// Logical
			case Token::Type::SymbolBang:
				return { Parser::Precedence::Prefix, &Parser::parse_unary, nullptr };

			// Literals
			case Token::Type::LiteralFloat:
			case Token::Type::LiteralIdentifier:
			case Token::Type::LiteralInteger:
			case Token::Type::LiteralString:
			case Token::Type::KeywordTrue:
			case Token::Type::KeywordFalse:
			case Token::Type::KeywordNull:
				return { Precedence::None, &Parser::parse_literal, nullptr };

			// Arithmetic
			case Token::Type::SymbolMinus:
				return { Parser::Precedence::Additive, &Parser::parse_unary, &Parser::parse_binary };
			case Token::Type::SymbolPlus:
				return { Parser::Precedence::Additive, nullptr, &Parser::parse_binary };
			case Token::Type::SymbolPercent:
			case Token::Type::SymbolSlash:
			case Token::Type::SymbolStar:
				return { Parser::Precedence::Multiplicative, nullptr, &Parser::parse_binary };
			case Token::Type::SymbolPlusPlus:
			case Token::Type::SymbolMinusMinus:
				return { Parser::Precedence::Prefix, &Parser::parse_unary, nullptr };

			// Other
			case Token::Type::KeywordCast:
				return { Precedence::None, &Parser::parse_cast, nullptr };
			case Token::Type::SymbolParenLeft:
				return { Precedence::Postfix, &Parser::parse_grouping, &Parser::parse_function_call };
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
				.type     = Token::Type::SpecialEndOfFile,
				.data     = Token::internal_name(Token::Type::SpecialEndOfFile),
				.location = SourceLocation{ last_token.location.row,
                                           static_cast<u32>(last_token.location.column + last_token.data.size()) }
			};
		}
		return *_current_token;
	}

}  // namespace soul::parser
