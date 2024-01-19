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
