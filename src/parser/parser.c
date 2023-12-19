#include "parser.h"

#include "ast/ast.h"
#include "lexer/token.h"

#include <stdlib.h>
#include <string.h>

// Statements
static soul_ast_node_t* parser_parse_statement(soul_parser_t*);
static soul_ast_node_t* parser_parse_variable_declaration_statement( soul_parser_t* parser);
static soul_ast_node_t* parser_parse_block_statement(soul_parser_t*);

// Expressions
static soul_precedence_rule_t parser_get_precedence_rule(soul_token_type_t type);
static soul_ast_node_t* parser_parse_expression(soul_parser_t*);
static soul_ast_node_t* parser_parse_expression_with_precedence(soul_parser_t*, soul_precedence_t);
static soul_ast_node_t* parser_parse_expression_statement(soul_parser_t*);

// Util
static void parser_advance(soul_parser_t*);
static void parser_synchronize(soul_parser_t*);
static bool parser_match(soul_parser_t*, soul_token_type_t);
static bool parser_match_any(soul_parser_t*, soul_token_type_t*, size_t);
static soul_token_t parser_peek(soul_parser_t*);
static soul_token_t parser_peek_prev(soul_parser_t*);
static soul_token_t parser_peek_next(soul_parser_t*);
static soul_token_t parser_require(soul_parser_t*, soul_token_type_t);
static bool is_sync_token(soul_token_type_t);
static bool is_assign_token(soul_token_type_t);

soul_parser_t soul_parser_create(void)
{
	soul_parser_t parser;
	parser.token_array = NULL;
	parser.current_token = 0;
	parser.had_panic = 0;
	parser.had_error = 0;
	return parser;
}

soul_ast_node_t* soul_parser_parse(soul_parser_t* parser,
								   soul_token_array_t* token_array)
{
	if(!parser || !token_array) return NULL;

	parser->token_array = token_array;
	soul_ast_node_array_t statements;
	soul_ast_node_array_initialize(&statements);
	while(soul_token_array_type_at(parser->token_array, parser->current_token) != soul_token_eof)
	{
		soul_ast_node_t* node = parser_parse_statement(parser);
		soul_ast_node_array_append(&statements, node);
		if(parser->had_panic) parser_synchronize(parser);
	}

	// TODO: Root node should be a function.
	return soul_ast_node_create_block_statement(statements);
}

//
// Private
//

static soul_ast_node_t* parser_parse_variable_declaration_statement(
		soul_parser_t* parser)
{
	// let (mut) <identifier> : <type> = <expression>;
	parser_require(parser, soul_token_let);

	// (Optional) Mutability
	bool is_mutable = parser_match(parser, soul_token_mut);
	if(is_mutable) parser_advance(parser);

	// Identifier
	soul_token_t identifier_token =
		parser_require(parser, soul_token_identifier);
	soul_ast_node_identifier_t identifier =
		soul_ast_node_identifier_create(identifier_token.start,
										identifier_token.length);

	// Type
	parser_require(parser, soul_token_colon);
	soul_token_t type_token = parser_require(parser, soul_token_identifier);
	soul_ast_node_identifier_t type =
		soul_ast_node_identifier_create(type_token.start, type_token.length);

	// Expression
	parser_require(parser, soul_token_equal);
	soul_ast_node_t* expr = parser_parse_expression(parser);

	return soul_ast_node_create_variable_decl_statement(identifier,
														type,
														expr,
														is_mutable);
}

static soul_ast_node_t* parser_parse_function_declaration_statement(
		soul_parser_t* parser)
{
	// fn <identifier> ((<params>)) :: <type> { <block> }
	parser_require(parser, soul_token_fn);

	// Identifier
	soul_token_t identifier_token =
		parser_require(parser, soul_token_identifier);
	soul_ast_node_identifier_t identifier =
		soul_ast_node_identifier_create(identifier_token.start,
										identifier_token.length);

	// (Optional) Parameters
	bool has_params = parser_match(parser, soul_token_paren_left);
	soul_ast_node_array_t params;
	soul_ast_node_array_initialize(&params);
	if(has_params)
	{
		parser_advance(parser); // Consume "("

		while(parser_match(parser, soul_token_identifier))
		{
			// <identifier> : <type>
			soul_token_t param_id_token =
				parser_require(parser, soul_token_identifier);
			soul_ast_node_identifier_t param_identifier =
				soul_ast_node_identifier_create(param_id_token.start,
												param_id_token.length);

			parser_require(parser, soul_token_colon);
			soul_token_t param_type_token =
				parser_require(parser, soul_token_identifier);
			soul_ast_node_identifier_t param_type =
				soul_ast_node_identifier_create(param_type_token.start,
												param_type_token.length);

			// TODO: This technically is not the best way to create function
			//       parameters, because Soul expects variables to be declared
			//       as they are defined, which in turn might cause an error
			//       later on.
			soul_ast_node_t* param =
				soul_ast_node_create_variable_decl_statement(param_identifier,
															 param_type,
															 NULL,
															 false);
			soul_ast_node_array_append(&params, param);
		}

		parser_require(parser, soul_token_paren_right); // Consume ")"
	} 

	// Type
	parser_require(parser, soul_token_double_colon);
	soul_token_t type_token = parser_require(parser, soul_token_identifier);
	soul_ast_node_identifier_t type =
		soul_ast_node_identifier_create(type_token.start, type_token.length);

	// Body
	soul_ast_node_t* body = parser_parse_block_statement(parser);

	return soul_ast_node_create_function_decl_statement(identifier,
														type,
														body,
														params);
}

static soul_ast_node_t* parser_parse_if_statement(soul_parser_t* parser)
{
	// if (<condition>) { <then_body> } else { <else_body> }
	parser_require(parser, soul_token_if);

	// Condition
	parser_require(parser, soul_token_paren_left);
	soul_ast_node_t* condition = parser_parse_expression(parser);
	parser_require(parser, soul_token_paren_right);

	// Then
	soul_ast_node_t* then_body = parser_parse_block_statement(parser);

	// (Optional) Else
	soul_ast_node_t* else_body = NULL;
	if(parser_match(parser, soul_token_else))
	{
		parser_advance(parser); // Consume "else"
		else_body = parser_parse_block_statement(parser);
	}

	return soul_ast_node_create_if_statement(condition, then_body, else_body);
}

static soul_ast_node_t* parser_parse_for_statement(soul_parser_t* parser)
{
	// for ( <initializer> ; <condition> ; <increment>) { <body> }
	parser_require(parser, soul_token_for);
	parser_require(parser, soul_token_paren_left);

	// (Optional) Initializer
	soul_ast_node_t* initializer = NULL;
	if(parser_match(parser, soul_token_let))
	{
		initializer = parser_parse_variable_declaration_statement(parser);
	}
	parser_require(parser, soul_token_semicolon);

	// (Optional) Condition
	soul_ast_node_t* condition = NULL;
	if(parser_match(parser, soul_token_identifier))
	{
		condition = parser_parse_expression(parser);
	}
	else 
	{
		// ...it is a while true loop!
		condition = soul_ast_node_create_boolean_literal_expression(true);
	}
	parser_require(parser, soul_token_semicolon);

	// (Optional) Increment 
	soul_ast_node_t* increment_statement = NULL;
	if(parser_match(parser, soul_token_identifier))
	{
		increment_statement = parser_parse_expression(parser);
	}
	parser_require(parser, soul_token_paren_right);

	// Body
	soul_ast_node_t* body = parser_parse_block_statement(parser);

	return soul_ast_node_create_for_statement(initializer,
											  condition,
											  increment_statement,
											  body);
}

static soul_ast_node_t* parser_parse_while_statement(soul_parser_t* parser)
{
	// while ((<condition>)) { <body> }
	parser_require(parser, soul_token_while);

	// (Optional) Condition
	soul_ast_node_t* condition = NULL;
	if(parser_match(parser, soul_token_paren_left))
	{
		condition = parser_parse_expression(parser);
		parser_require(parser, soul_token_paren_right);
	}
	else 
	{
		// ...it is a while true loop!
		condition = soul_ast_node_create_boolean_literal_expression(true);
	}

	// Body
	soul_ast_node_t* body = parser_parse_block_statement(parser);

	return soul_ast_node_create_while_statement(condition, body);
}

static soul_ast_node_t* parser_parse_block_statement(soul_parser_t* parser)
{
	// { <statements> }
	parser_require(parser, soul_token_brace_left);

	soul_ast_node_array_t statements;
	while(parser_peek(parser).type != soul_token_brace_right)
	{
		soul_ast_node_array_append(&statements,
								   parser_parse_statement(parser));
		// NOTE: Prevent trying to parse unterminated blocks.
		// TODO: Propagate error.
		if(parser_peek(parser).type == soul_token_eof) break;
	}
	parser_require(parser, soul_token_brace_right);

	return soul_ast_node_create_block_statement(statements);
}

static soul_ast_node_t* parser_parse_return_statement(soul_parser_t* parser)
{
	// return (<expr>);
	parser_require(parser, soul_token_return);

	// (Optional) Expression;
	soul_ast_node_t* return_expr = NULL;
	if(!parser_match(parser, soul_token_semicolon))
	{
		return_expr = parser_parse_expression(parser);
	}
	parser_require(parser, soul_token_semicolon);

	return soul_ast_node_create_return_statement(return_expr);
}

static soul_ast_node_t* parser_parse_statement(soul_parser_t* parser)
{
	switch(parser_peek(parser).type)
	{
		case soul_token_let:
			return parser_parse_variable_declaration_statement(parser);
		case soul_token_fn:
			return parser_parse_function_declaration_statement(parser);
		case soul_token_if:
			return parser_parse_if_statement(parser);
		case soul_token_for:
			return parser_parse_for_statement(parser);
		case soul_token_while:
			return parser_parse_while_statement(parser);
		case soul_token_paren_left:
			return parser_parse_block_statement(parser);
		case soul_token_return:
			return parser_parse_return_statement(parser);
		default:
			break;
	}
	soul_ast_node_t* statement = parser_parse_expression_statement(parser);
	parser_require(parser, soul_token_semicolon);
	return statement;
}

static soul_ast_node_t* parser_parse_literal_expression(soul_parser_t* parser)
{
	soul_token_t token = parser_peek_prev(parser);
	soul_ast_node_t* node = NULL;
	switch(token.type)
	{
		case soul_token_identifier:
			{
				soul_ast_node_identifier_t val = 
					soul_ast_node_identifier_create(token.start, token.length);
				node = soul_ast_node_create_variable_literal_expression(val);
				break;
			}
		case soul_token_number:
			{
				node = soul_ast_node_create_number_literal_expression(0);
				break;
			}
		case soul_token_string:
			{
				soul_ast_node_identifier_t val = 
					soul_ast_node_identifier_create(token.start, token.length);
				node = soul_ast_node_create_string_literal_expression(val);
				break;
			}
		case soul_token_true:
		case soul_token_false:
		{
			bool val = token.type == soul_token_true;
			node = soul_ast_node_create_boolean_literal_expression(val);
			break;
		}
		default:
			// @TODO Propagate error.
			break;
	}

	return node;
}

static soul_ast_node_t* parser_parse_assignment_expression(soul_parser_t* parser)
{
	soul_ast_node_t* lhs = parser_parse_expression(parser);
	parser_advance(parser); // Consume the assignment token.
	soul_ast_node_t* rhs = parser_parse_expression(parser);
	return soul_ast_node_create_assign_expression(lhs, rhs);
}

static soul_ast_node_t* parser_parse_unary_expression(soul_parser_t* parser)
{
	soul_token_t token = parser_peek_prev(parser);
	parser_advance(parser);
	soul_ast_node_t* val = parser_parse_literal_expression(parser);
	soul_ast_node_operator_t op = soul_token_type_to_operator(token.type);
	return soul_ast_node_create_unary_expression(val, op);
}

static soul_ast_node_t* parser_parse_binary_expression(soul_parser_t* parser, soul_ast_node_t* lhs)
{
	soul_token_t token = parser_peek_prev(parser);
	soul_precedence_rule_t rule = parser_get_precedence_rule(token.type);
	soul_ast_node_t* rhs = parser_parse_expression_with_precedence(parser, (rule.precedence + 1));
	soul_ast_node_operator_t op = soul_token_type_to_operator(token.type);
	return soul_ast_node_create_binary_expression(lhs, rhs, op);
}

static soul_precedence_rule_t parser_get_precedence_rule(soul_token_type_t type)
{
	if(soul_is_literal_token(type))
		return (soul_precedence_rule_t) { soul_prec_none, parser_parse_literal_expression, NULL };

	switch(type)
	{
		case soul_token_minus:
			return (soul_precedence_rule_t)
				{
					soul_prec_additive,
					parser_parse_unary_expression,
					parser_parse_binary_expression,
				};
		case soul_token_plus:
			return (soul_precedence_rule_t)
				{
					soul_prec_additive,
					NULL,
					parser_parse_binary_expression,
				};
		case soul_token_star:
		case soul_token_slash:
			return (soul_precedence_rule_t)
				{
					soul_prec_multiplicative,
					NULL,
					parser_parse_binary_expression,
				};
		default:
			// NOTE: No precedence.
			return (soul_precedence_rule_t) { soul_prec_none, NULL, NULL };
	}
}

static soul_ast_node_t* parser_parse_expression_statement(soul_parser_t* parser)
{
	soul_token_type_t next_type = parser_peek_next(parser).type;

	/* if (is_assign_token(next_type)) */
		/* return parser_parse_assignment_expression(parser); */

	soul_ast_node_t* expr = parser_parse_expression(parser);
	return soul_ast_node_create_expression_statement(expr);
}

static soul_ast_node_t* parser_parse_expression_with_precedence(
	soul_parser_t* parser, soul_precedence_t precedence)
{
	parser_advance(parser); // Skip operand token.

	soul_token_type_t previous_token = parser_peek_prev(parser).type;
	soul_prefix_precedence_fn prefix_rule =
		parser_get_precedence_rule(previous_token).prefix;

	if(!prefix_rule)
	{
		// TODO: Propagate error: expected expression.
		return NULL;
	}
	soul_ast_node_t* prefix_expr = prefix_rule(parser);

	while(precedence <= parser_get_precedence_rule(
				parser_peek(parser).type).precedence)
	{
		parser_advance(parser);
		previous_token = parser_peek_prev(parser).type;
		soul_infix_precedence_fn infix_rule = 
			parser_get_precedence_rule(previous_token).infix;
		prefix_expr = infix_rule(parser, prefix_expr);
	}

	return prefix_expr;
}

static soul_ast_node_t* parser_parse_expression(soul_parser_t* parser)
{
	// NOTE: Starting precedence has to be at least 1 level higher than
	//       no precedence.
	return parser_parse_expression_with_precedence(parser, soul_prec_none + 1);
}

static void parser_advance(soul_parser_t* parser)
{
	if(parser->current_token + 1 > parser->token_array->size)
	{
		// @TODO: Error
		return;
	}
	parser->current_token++;
	if(soul_token_array_type_at(parser->token_array, parser->current_token) == soul_token_error)
	{
		// @TODO: Error
	}
}

static void parser_synchronize(soul_parser_t* parser)
{
	parser->had_panic = false;

	while(parser_match(parser, soul_token_eof))
	{
		if(!is_sync_token(soul_token_array_type_at(parser->token_array, parser->current_token)))
		{
			parser_advance(parser);
		}
		break; // Synchronized!
	}
}

static bool parser_match(soul_parser_t* parser, soul_token_type_t type)
{
	return soul_token_array_type_at(parser->token_array,
									parser->current_token) == type;
}

static bool parser_match_any(soul_parser_t* parser,
							 soul_token_type_t* types,
							 size_t count)
{
	soul_token_type_t current_type =
		soul_token_array_type_at(parser->token_array, parser->current_token);
	for(size_t i = 0; i < count; ++i)
	{
		if(current_type == types[i])
		{
			return true;
		}
	}
	return false;
}

static soul_token_t parser_peek(soul_parser_t* parser)
{
	return soul_token_array_at(parser->token_array, parser->current_token);
}

static soul_token_t parser_peek_prev(soul_parser_t* parser)
{
#ifdef NDEBUG
	if (parser->current_token < 1)
	{
		// TODO: better erorr message
		// TODO: better creation of error tokens.
		const char* error_string = "cannot peek at a negative index";
		soul_token_t error = {
			.type = soul_token_error,
			.start = error_string,
			.length = strlen(error_string),
		};
		return error;
	}
#endif
	return soul_token_array_at(parser->token_array, parser->current_token - 1);
}

static soul_token_t parser_peek_next(soul_parser_t* parser)
{
#ifdef NDEBUG
	if (parser->current_token + 1 > parser->token_array->size)
	{
		// TODO: better erorr message
		// TODO: better creation of error tokens.
		const char* error_string = "cannot peek forward, because index is out of range";
		soul_token_t error = {
			.type = soul_token_error,
			.start = error_string,
			.length = strlen(error_string),
		};
		return error;
	}
#endif
	return soul_token_array_at(parser->token_array, parser->current_token + 1);
}

static soul_token_t parser_require(soul_parser_t* parser, soul_token_type_t type)
{
	if(parser_peek(parser).type != type)
	{
		// TODO: better creation of error tokens.
		const char* error_string = "required token is missing";
		soul_token_t error = {
			.type = soul_token_error,
			.start = error_string,
			.length = strlen(error_string),
		};
		return error;
	}
	parser_advance(parser);
	return parser_peek_prev(parser);
}

static bool is_sync_token(soul_token_type_t type)
{
	// @TODO
	return type == soul_token_semicolon
		|| type == soul_token_brace_right;
}

static bool is_assign_token(soul_token_type_t type)
{
	// @TODO
	return type == soul_token_equal;
}

#if 0
	ast_node* parser::parse_statement()
	{
		switch(peek().type)
		{
			case token_type::token_define:
				{
					ast_node* s = parse_define_statement();
					require(token_type::token_semicolon);
					return s;
				}
			case token_type::token_let:
				{
					ast_node* s = parse_variable_declaration_statement();
					require(token_type::token_semicolon);
					return s;
				}
			case token_type::token_fn: 
				return parse_function_declaration_statement();
			case token_type::token_if: 
				return parse_if_statement();
			case token_type::token_for: 
				return parse_for_statement();
			case token_type::token_while: 
				return parse_while_statement();
			case token_type::token_brace_left:
				return parse_body_statement();
			default:
				break;
		}

		// Expression statements.
		ast_node* statement = parse_expression_statement();
		require(token_type::token_semicolon);
		return statement;
	}

	ast_node* parser::parse_define_statement()
	{
		require(token_type::token_define); // Consume define declaration

		// Identifier
		token identifier_token = require(token_type::token_identifier);
		ast_node_identifier identifier = std::string(identifier_token.start, identifier_token.size);

		// Expression
		ast_node* expr = parse_expression();

		ast_node* node = create_node(ast_node_type::ast_stmt_define_decl);
		node->as.define_decl_stmt.id = identifier;
		node->as.define_decl_stmt.expr = expr;
		return node;
	}

	ast_node* parser::parse_variable_declaration_statement()
	{
		require(token_type::token_let); // Consume variable declaration token

		// (Optional) Mutability
		bool is_mutable = false;
		if(peek().type == token_type::token_mut)
		{
			is_mutable = true;
			advance();
		}

		// Identifier
		token identifier_token = require(token_type::token_identifier);
		ast_node_identifier identifier = std::string(identifier_token.start, identifier_token.size);

		require(token_type::token_colon); // Consume type declaration

		// Token type

		require(token_type::token_equal); // Consume assignment operator

		// Expression
		ast_node* expr = parse_expression();

		ast_node* node = create_node(ast_node_type::ast_stmt_variable_decl);
		node->as.variable_decl_stmt.identifier = std::move(identifier);
		node->as.variable_decl_stmt.val = expr;
		return node;
	}

	ast_node* parser::parse_function_declaration_statement()
	{
		require(token_type::token_fn); // Consume function declaration token

		// Identifier
		token identifier_token = require(token_type::token_identifier);
		ast_node_identifier identifier = std::string(identifier_token.start, identifier_token.size);

		// (Optional) parameters
		// @TODO Skipped for now.
		if(match(token_type::token_paren_left))
		{
			advance(); // Consume parametrization start token.

			while(match(token_type::token_identifier))
			{
				advance(); // @TODO Identifier
				require(token_type::token_colon);
				advance(); // @TODO Type

				if(peek().type != token_type::token_comma) break;
			}

			require(token_type::token_paren_right); // Consume parametrization stop token;
		}

		require(token_type::token_double_colon); // Consume type declaration token

		// Return type
		// @TODO
		token type_token = require(token_type::token_identifier);
		ast_node_identifier return_type = std::string(type_token.start, type_token.size);

		// Function body
		ast_node* body = parse_body_statement();

		ast_node* node = create_node(ast_node_type::ast_stmt_function_decl);
		node->as.function_decl_stmt.identifier = identifier;
		node->as.function_decl_stmt.body = body;
		return node;
	}

	ast_node* parser::parse_if_statement()
	{
		require(token_type::token_if); // Consume if statement token

		// Condition
		require(token_type::token_paren_left);
		ast_node* condition = parse_expression();
		require(token_type::token_paren_right);

		// Then block
		ast_node* then_stmt = parse_body_statement();

		// (Optional) Else block
		ast_node* else_stmt = nullptr;
		if(match(token_type::token_else))
		{
			advance(); // Consume else token
			else_stmt = parse_body_statement();
		}

		ast_node* node = create_node(ast_node_type::ast_stmt_if);
		node->as.if_stmt.condition = condition;
		node->as.if_stmt.then_stmt = then_stmt;
		node->as.if_stmt.else_stmt = else_stmt;

		return node;
	}

	ast_node* parser::parse_for_statement()
	{
		require(token_type::token_for); // Consumre for statement token
		require(token_type::token_paren_left);

		// (Optional) Mutable variable declaration
		ast_node* init = nullptr;
		if(!match(token_type::token_semicolon))
		{
			init = parse_variable_declaration_statement();
		}

		// (Optional) Condition
		ast_node* condition = nullptr;
		if(!match(token_type::token_semicolon))
		{
			condition = parse_expression();
		}

		// (Optional) Iteration expression
		ast_node* expr = nullptr;
		if(!match(token_type::token_paren_right))
		{
			expr = parse_expression();
		}

		require(token_type::token_paren_right);

		// Body
		ast_node* body = parse_body_statement();

		ast_node* node = create_node(ast_node_type::ast_stmt_for);
		node->as.for_stmt.initializer = init;
		node->as.for_stmt.condition = condition;
		node->as.for_stmt.expr = expr;
		node->as.for_stmt.body = body;
		return node;
	}

	ast_node* parser::parse_while_statement()
	{
		require(token_type::token_while); // Consume while statement token

		ast_node* condition = nullptr;
		bool has_params = match(token_type::token_paren_left);
		if(has_params)
		{
			advance();
			condition = parse_expression();
			require(token_type::token_paren_right);
		}
		else
		{
			// NOTE: Parameterless variant has form: while { <body > }
			condition = parse_expression();
			advance();
		}

		// Body
		ast_node* body = parse_body_statement();

		ast_node* node = create_node(ast_node_type::ast_stmt_while);
		node->as.while_stmt.condition = condition;
		node->as.while_stmt.body = body;
		return node;
	}

	ast_node* parser::parse_body_statement()
	{
		require(token_type::token_brace_left); // Consume block start token

		std::vector<ast_node*> statements;
		while(peek().type != token_type::token_brace_right)
		{
			statements.emplace_back(parse_statement());
			// NOTE: Prevent trying to parse unterminated blocks.
			if(peek().type == token_type::token_eof) break;
		}

		require(token_type::token_brace_right); // Consume block end token

		ast_node* node = create_node(ast_node_type::ast_stmt_block);
		node->as.block_stmt.stmts = std::move(statements);
		return node;
	}

	ast_node* parser::parse_expression()
	{
		// NOTE: Starting precedence has to be at least 1 higher than no precedence.
		return parse_precedence_expression((precedence)(precedence::prec_none + 1));
	}

	ast_node* parser::parse_expression_statement()
	{
		token_type next_type = peek_next().type;

		if(is_assign_token(next_type))
			return parse_assign_expression_statement();

		ast_node* expr = parse_expression();
		expr->type = ast_node_type::ast_expr_stmt;
		expr->as.expr_stmt.expr = expr;
		return expr;
	}

	ast_node* parser::parse_precedence_expression(precedence prec)
	{
		advance(); // Skip op token.

		prefix_precedence_fn prefix_rule = get_precedence_rule(peek_prev().type).prefix;
		if(!prefix_rule)
		{
			// @TODO Propate error: expected expression
			return nullptr;
		}
		ast_node* prefix_expr = (this->*prefix_rule)();

		while(prec <= get_precedence_rule(tokens[current_token].type).precedence)
		{
			advance();
			infix_precedence_fn infix_rule = get_precedence_rule(peek_prev().type).infix;
			prefix_expr = (this->*infix_rule)(prefix_expr);
		}

		return prefix_expr;
	}

	ast_node* parser::parse_binary_expression(ast_node* lhs)
	{
		token op = peek_prev();
		precedence_rule rule = get_precedence_rule(op.type);
		ast_node* rhs = parse_precedence_expression((precedence)(rule.precedence + 1));
		ast_node* node = create_node(ast_node_type::ast_expr_binary);
		node->as.binary_expr.op = to_node_operator(op.type);
		node->as.binary_expr.lval = lhs;
		node->as.binary_expr.rval = rhs;
		return node;
	}

	ast_node* parser::parse_unary_expression()
	{
		token op = peek_prev();
		advance();
		ast_node* val = parse_literal_expression();
		ast_node* node = create_node(ast_node_type::ast_expr_unary);
		node->as.unary_expr.op = to_node_operator(op.type);
		node->as.unary_expr.val = val;
		return node;
	}

	ast_node* parser::parse_literal_expression()
	{
		ast_node* node = nullptr;
		token t = peek_prev();
		switch(t.type)
		{
			case token_type::token_identifier:
				{
					node->as.var_lit_expr.val = std::string(t.start, t.size);
					return node;
				}
			case token_type::token_number:
				{
					// @TODO Parsing all number types.
					//       Currently we asume that was can parse only integers,
					//       but Soul supports real numbers too. Figure a way to parse both of them.
					node->as.number_lit_expr.type = ast_node_number_literal::number_type::integer;
					node->as.number_lit_expr.as.int_val  = (int64_t)strtoll(t.start, nullptr, 0);
					return node;
				}
			case token_type::token_string:
				{
					node->as.string_lit_expr.val = std::string(t.start + 1, t.size - 2);
					return node;
				}
			case token_type::token_true:
			case token_type::token_false:
				{
					node->as.bool_lit_expr.val = t.type != token_type::token_false;
					return node;
				}
			default:
			   break;
		}

		// @TODO Propagate error. Expected literal.
		return node;
	}

	ast_node* parser::parse_assign_expression_statement()
	{
		ast_node* lhs = parse_expression();
		require(token_type::token_equal); // Consume assigment operator
		ast_node* rhs = parse_expression();

		ast_node* expr = create_node(ast_node_type::ast_expr_assign);
		expr->as.assign_expr.lhs = lhs;
		expr->as.assign_expr.lhs = rhs;
		return expr;
	}

	precedence_rule parser::get_precedence_rule(token_type type)
	{
		if(is_literal_token(type))
			return (precedence_rule) { precedence::prec_none, &parser::parse_literal_expression, nullptr };

		switch(type)
		{
			case token_type::token_minus:
				return (precedence_rule)
					{
						precedence::prec_additive,
						&parser::parse_unary_expression,
						&parser::parse_binary_expression,
					};
			case token_type::token_plus:
				return (precedence_rule)
					{
						precedence::prec_additive,
						nullptr,
						&parser::parse_binary_expression,
					};
			case token_type::token_star:
			case token_type::token_slash:
				return (precedence_rule)
					{
						precedence::prec_multiplicative,
						nullptr,
						&parser::parse_binary_expression,
					};
			default:
				// NOTE: No precedence
				return (precedence_rule) { precedence::prec_none, nullptr, nullptr };
		}
	}
#endif
