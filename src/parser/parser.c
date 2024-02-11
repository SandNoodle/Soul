#include "parser.h"

#include <stdlib.h>
#include <string.h>
#include <stdio.h>

typedef enum soul_precedence_t : uint8_t
{
	soul_prec_none = 0,
	soul_prec_assign,         // =
	soul_prec_or,             // ||
	soul_prec_and,            // &&
	soul_prec_equal,          // == !=
	soul_prec_compare,        // < > <= =>
	soul_prec_additive,       // + -
	soul_prec_multiplicative, // * /
	soul_prec_unary,          // ! -
	soul_prec_call,           // @TODO
	soul_prec_primary,
} soul_precedence_t;

typedef struct soul_ast_node_t* (*soul_prefix_precedence_fn)(soul_parser_t*);
typedef struct soul_ast_node_t* (*soul_infix_precedence_fn)(soul_parser_t*, soul_ast_node_t*);

typedef struct soul_precedence_rule_t soul_precedence_rule_t;
struct soul_precedence_rule_t
{
	soul_precedence_t precedence;
	soul_prefix_precedence_fn prefix;
	soul_infix_precedence_fn infix;
};

// clang-format off
// Statements
static soul_ast_node_t* parse_statement(soul_parser_t*);
static soul_ast_node_t* parse_variable_declaration_statement(soul_parser_t* parser);
static soul_ast_node_t* parse_block_statement(soul_parser_t*);

// Expressions
static soul_precedence_rule_t get_precedence_rule(soul_token_type_t type);
static soul_ast_node_t* parse_expression(soul_parser_t*);
static soul_ast_node_t* parse_expression_with_precedence(soul_parser_t*, soul_precedence_t);
static soul_ast_node_t* parse_expression_statement(soul_parser_t*);

// Util
static void advance(soul_parser_t*);
static void synchronize(soul_parser_t*);
static bool match(soul_parser_t*, soul_token_type_t);
static bool match_any(soul_parser_t*, soul_token_type_t*, size_t);
static soul_token_t peek(soul_parser_t*);
static soul_token_t peek_prev(soul_parser_t*);
static soul_token_t peek_next(soul_parser_t*);
static soul_token_t require(soul_parser_t*, soul_token_type_t);
// clang-format on

soul_parser_t soul_parser_create(soul_allocator_t* allocator)
{
	soul_parser_t parser;
	parser.token_array   = NULL;
	parser.current_token = 0;
	parser.had_panic     = 0;
	parser.had_error     = 0;
	parser.allocator     = allocator;
	return parser;
}

soul_ast_node_t* soul_parser_parse(soul_parser_t* parser,
                                   soul_token_array_t* token_array)
{
	if (!parser || !token_array) return NULL;

	parser->token_array = token_array;
	soul_ast_node_array_t statements = soul_ast_node_array_create(parser->allocator);
	while (soul_token_array_type_at(parser->token_array, parser->current_token)
	       != soul_token_eof)
	{
		const soul_token_t token = soul_token_array_at(parser->token_array, parser->current_token);
		soul_ast_node_t* node = parse_statement(parser);
		soul_ast_node_array_append(&statements, node, parser->allocator);
		if (parser->had_panic) synchronize(parser);
	}

	// TODO: Root node should be a function.
	return soul_ast_node_create_block_statement(statements, parser->allocator);
}

//
// Private
//

static soul_ast_node_t* parse_variable_declaration_statement(
    soul_parser_t* parser)
{
	// let (mut) <identifier> : <type> = <expression>;
	require(parser, soul_token_let);

	// (Optional) Mutability
	bool is_mutable = match(parser, soul_token_mut);
	if (is_mutable) advance(parser);

	// Identifier
	soul_token_t identifier_token
	    = require(parser, soul_token_identifier);
	soul_ast_node_identifier_t identifier = soul_ast_node_identifier_create(
	    identifier_token.start, identifier_token.length, parser->allocator);

	// Type
	require(parser, soul_token_colon);
	soul_token_t type_token = require(parser, soul_token_identifier);
	soul_ast_node_identifier_t type = soul_ast_node_identifier_create(
		type_token.start, type_token.length, parser->allocator);

	// Expression
	require(parser, soul_token_equal);
	soul_ast_node_t* expr = parse_expression(parser);
	require(parser, soul_token_semicolon);

	return soul_ast_node_create_variable_decl_statement(identifier, type, expr,
	                                                    is_mutable, parser->allocator);
}

static soul_ast_node_t* parse_function_declaration_statement(
    soul_parser_t* parser)
{
	// fn <identifier> ((<params>)) :: <type> { <block> }
	require(parser, soul_token_fn);

	// Identifier
	soul_token_t identifier_token
	    = require(parser, soul_token_identifier);
	soul_ast_node_identifier_t identifier = soul_ast_node_identifier_create(
	    identifier_token.start, identifier_token.length, parser->allocator);

	// (Optional) Parameters
	bool has_params = match(parser, soul_token_paren_left);
	soul_ast_node_array_t params = soul_ast_node_array_create(parser->allocator);
	if (has_params)
	{
		advance(parser); // Consume "("

		while (match(parser, soul_token_identifier))
		{
			// <identifier> : <type>
			soul_token_t param_id_token
			    = require(parser, soul_token_identifier);
			soul_ast_node_identifier_t param_identifier
			    = soul_ast_node_identifier_create(param_id_token.start,
			                                      param_id_token.length,
			                                      parser->allocator);

			require(parser, soul_token_colon);
			soul_token_t param_type_token
			    = require(parser, soul_token_identifier);
			soul_ast_node_identifier_t param_type
			    = soul_ast_node_identifier_create(param_type_token.start,
			                                      param_type_token.length,
			                                      parser->allocator);

			// TODO: This technically is not the best way to create function
			//       parameters, because Soul expects variables to be declared
			//       as they are defined, which in turn might cause an error
			//       later on.
			soul_ast_node_t* param
			    = soul_ast_node_create_variable_decl_statement(
			        param_identifier, param_type, NULL, false, parser->allocator);
			soul_ast_node_array_append(&params, param, parser->allocator);
		}

		require(parser, soul_token_paren_right); // Consume ")"
	}

	// Type
	require(parser, soul_token_double_colon);
	soul_token_t type_token = require(parser, soul_token_identifier);
	soul_ast_node_identifier_t type
	    = soul_ast_node_identifier_create(type_token.start, type_token.length, parser->allocator);

	// Body
	soul_ast_node_t* body = parse_block_statement(parser);

	return soul_ast_node_create_function_decl_statement(identifier, type, body,
	                                                    params, parser->allocator);
}

static soul_ast_node_t* parse_if_statement(soul_parser_t* parser)
{
	// if (<condition>) { <then_body> } else { <else_body> }
	require(parser, soul_token_if);

	// Condition
	require(parser, soul_token_paren_left);
	soul_ast_node_t* condition = parse_expression(parser);
	require(parser, soul_token_paren_right);

	// Then
	soul_ast_node_t* then_body = parse_block_statement(parser);

	// (Optional) Else
	soul_ast_node_t* else_body = NULL;
	if (match(parser, soul_token_else))
	{
		advance(parser); // Consume "else"
		else_body = parse_block_statement(parser);
	}

	return soul_ast_node_create_if_statement(condition, then_body, else_body, parser->allocator);
}

static soul_ast_node_t* parse_for_statement(soul_parser_t* parser)
{
	// for ( <initializer> ; <condition> ; <increment>) { <body> }
	require(parser, soul_token_for);
	require(parser, soul_token_paren_left);

	// (Optional) Initializer
	soul_ast_node_t* initializer = NULL;
	if (match(parser, soul_token_let))
	{
		initializer = parse_variable_declaration_statement(parser);
	}
	require(parser, soul_token_semicolon);

	// (Optional) Condition
	soul_ast_node_t* condition = NULL;
	if (match(parser, soul_token_identifier))
	{
		condition = parse_expression(parser);
	}
	else
	{
		// ...it is a while true loop!
		condition = soul_ast_node_create_boolean_literal_expression(true, parser->allocator);
	}
	require(parser, soul_token_semicolon);

	// (Optional) Increment
	soul_ast_node_t* increment_statement = NULL;
	if (match(parser, soul_token_identifier))
	{
		increment_statement = parse_expression(parser);
	}
	require(parser, soul_token_paren_right);

	// Body
	soul_ast_node_t* body = parse_block_statement(parser);

	return soul_ast_node_create_for_statement(initializer, condition,
	                                          increment_statement, body, parser->allocator);
}

static soul_ast_node_t* parse_while_statement(soul_parser_t* parser)
{
	// while ((<condition>)) { <body> }
	require(parser, soul_token_while);

	// (Optional) Condition
	soul_ast_node_t* condition = NULL;
	if (match(parser, soul_token_paren_left))
	{
		condition = parse_expression(parser);
		require(parser, soul_token_paren_right);
	}
	else
	{
		// ...it is a while true loop!
		condition = soul_ast_node_create_boolean_literal_expression(true, parser->allocator);
	}

	// Body
	soul_ast_node_t* body = parse_block_statement(parser);

	return soul_ast_node_create_while_statement(condition, body, parser->allocator);
}

static soul_ast_node_t* parse_block_statement(soul_parser_t* parser)
{
	// { <statements> }
	require(parser, soul_token_brace_left);

	soul_ast_node_array_t statements;
	while (peek(parser).type != soul_token_brace_right)
	{
		soul_ast_node_array_append(&statements, parse_statement(parser), parser->allocator);
		// NOTE: Prevent trying to parse unterminated blocks.
		// TODO: Propagate error.
		if (peek(parser).type == soul_token_eof) break;
	}
	require(parser, soul_token_brace_right);

	return soul_ast_node_create_block_statement(statements, parser->allocator);
}

static soul_ast_node_t* parse_return_statement(soul_parser_t* parser)
{
	// return (<expr>);
	require(parser, soul_token_return);

	// (Optional) Expression;
	soul_ast_node_t* return_expr = NULL;
	if (!match(parser, soul_token_semicolon))
	{
		return_expr = parse_expression(parser);
	}
	require(parser, soul_token_semicolon);

	return soul_ast_node_create_return_statement(return_expr, parser->allocator);
}

static soul_ast_node_t* parse_statement(soul_parser_t* parser)
{
	switch (peek(parser).type)
	{
		case soul_token_let:
			return parse_variable_declaration_statement(parser);
		case soul_token_fn:
			return parse_function_declaration_statement(parser);
		case soul_token_if:
			return parse_if_statement(parser);
		case soul_token_for:
			return parse_for_statement(parser);
		case soul_token_while:
			return parse_while_statement(parser);
		case soul_token_brace_left:
			return parse_block_statement(parser);
		case soul_token_return:
			return parse_return_statement(parser);
		default:
			break;
	}
	soul_ast_node_t* statement = parse_expression_statement(parser);
	require(parser, soul_token_semicolon);
	return statement;
}

static soul_ast_node_t* parse_literal_expression(soul_parser_t* parser)
{
	soul_token_t token    = peek_prev(parser);
	soul_ast_node_t* node = NULL;
	switch (token.type)
	{
		case soul_token_identifier: {
			soul_ast_node_identifier_t val = soul_ast_node_identifier_create(
			    token.start, token.length, parser->allocator);
			node = soul_ast_node_create_variable_literal_expression(val, parser->allocator);
			break;
		}
		case soul_token_number: {
			node = soul_ast_node_create_number_literal_expression(0, parser->allocator);
			break;
		}
		case soul_token_string: {
			soul_ast_node_identifier_t val
			    = soul_ast_node_identifier_create(token.start, token.length, parser->allocator);
			node = soul_ast_node_create_string_literal_expression(val, parser->allocator);
			break;
		}
		case soul_token_true:
		case soul_token_false: {
			node = soul_ast_node_create_boolean_literal_expression(token.type == soul_token_true, parser->allocator);
			break;
		}
		default:
			// @TODO Propagate error.
			break;
	}

	return node;
}

static soul_ast_node_t* parse_assignment_expression(
    soul_parser_t* parser)
{
	soul_ast_node_t* lhs = parse_expression(parser);
	advance(parser); // Consume the assignment token.
	soul_ast_node_t* rhs = parse_expression(parser);
	return soul_ast_node_create_assign_expression(lhs, rhs, parser->allocator);
}

static soul_ast_node_t* parse_unary_expression(soul_parser_t* parser)
{
	soul_token_t token = peek_prev(parser);
	advance(parser);
	soul_ast_node_t* val        = parse_literal_expression(parser);
	soul_ast_node_operator_t op = soul_token_type_to_operator(token.type);
	return soul_ast_node_create_unary_expression(val, op, parser->allocator);
}

static soul_ast_node_t* parse_binary_expression(soul_parser_t* parser,
                                                       soul_ast_node_t* lhs)
{
	soul_token_t token          = peek_prev(parser);
	soul_precedence_rule_t rule = get_precedence_rule(token.type);
	soul_ast_node_t* rhs        = parse_expression_with_precedence(
        parser, (rule.precedence + 1));
	soul_ast_node_operator_t op = soul_token_type_to_operator(token.type);
	return soul_ast_node_create_binary_expression(lhs, rhs, op, parser->allocator);
}

static soul_precedence_rule_t get_precedence_rule(soul_token_type_t type)
{
	if (soul_is_literal_token(type))
		return (soul_precedence_rule_t){soul_prec_none,
		                                parse_literal_expression, NULL};

	switch (type)
	{
		case soul_token_minus:
			return (soul_precedence_rule_t){
			    soul_prec_additive,
			    parse_unary_expression,
			    parse_binary_expression,
			};
		case soul_token_plus:
			return (soul_precedence_rule_t){
			    soul_prec_additive,
			    NULL,
			    parse_binary_expression,
			};
		case soul_token_star:
		case soul_token_slash:
			return (soul_precedence_rule_t){
			    soul_prec_multiplicative,
			    NULL,
			    parse_binary_expression,
			};
		default:
			// NOTE: No precedence.
			return (soul_precedence_rule_t){soul_prec_none, NULL, NULL};
	}
}

static soul_ast_node_t* parse_expression_statement(soul_parser_t* parser)
{
	soul_token_type_t next_type = peek_next(parser).type;

	if (soul_is_assign_token(next_type))
		return parse_assignment_expression(parser);

	soul_ast_node_t* expr = parse_expression(parser);
	return soul_ast_node_create_expression_statement(expr, parser->allocator);
}

static soul_ast_node_t* parse_expression_with_precedence(
    soul_parser_t* parser, soul_precedence_t precedence)
{
	advance(parser); // Skip operand token.

	soul_token_type_t previous_token = peek_prev(parser).type;
	soul_prefix_precedence_fn prefix_rule
	    = get_precedence_rule(previous_token).prefix;

	if (!prefix_rule)
	{
		// TODO: Propagate error: expected expression.
		return NULL;
	}
	soul_ast_node_t* prefix_expr = prefix_rule(parser);

	while (precedence
	       <= get_precedence_rule(peek(parser).type).precedence)
	{
		advance(parser);
		previous_token = peek_prev(parser).type;
		soul_infix_precedence_fn infix_rule
		    = get_precedence_rule(previous_token).infix;
		prefix_expr = infix_rule(parser, prefix_expr);
	}

	return prefix_expr;
}

static soul_ast_node_t* parse_expression(soul_parser_t* parser)
{
	// NOTE: Starting precedence has to be at least 1 level higher than
	//       no precedence.
	return parse_expression_with_precedence(parser, soul_prec_none + 1);
}

static void advance(soul_parser_t* parser)
{
	if (parser->current_token + 1 > parser->token_array->size)
	{
		// @TODO: Error
		return;
	}
	parser->current_token++;
	if (soul_token_array_type_at(parser->token_array, parser->current_token)
	    == soul_token_error)
	{
		// @TODO: Error
	}
}

static void synchronize(soul_parser_t* parser)
{
	parser->had_panic = false;

	while (match(parser, soul_token_eof))
	{
		if (!soul_is_sync_token(soul_token_array_type_at(
		        parser->token_array, parser->current_token)))
		{
			advance(parser);
		}
		break; // Synchronized!
	}
}

static bool match(soul_parser_t* parser, soul_token_type_t type)
{
	return soul_token_array_type_at(parser->token_array, parser->current_token)
	    == type;
}

static bool match_any(soul_parser_t* parser, soul_token_type_t* types,
                             size_t count)
{
	soul_token_type_t current_type
	    = soul_token_array_type_at(parser->token_array, parser->current_token);
	for (size_t i = 0; i < count; ++i)
	{
		if (current_type == types[i]) { return true; }
	}
	return false;
}

static soul_token_t peek(soul_parser_t* parser)
{
	return soul_token_array_at(parser->token_array, parser->current_token);
}

static soul_token_t peek_prev(soul_parser_t* parser)
{
#ifdef NDEBUG
	if (parser->current_token < 1)
	{
		// TODO: better erorr message
		// TODO: better creation of error tokens.
		const char* error_string = "cannot peek at a negative index";
		soul_token_t error       = {
		          .type   = soul_token_error,
		          .start  = error_string,
		          .length = strlen(error_string),
        };
		return error;
	}
#endif
	return soul_token_array_at(parser->token_array, parser->current_token - 1);
}

static soul_token_t peek_next(soul_parser_t* parser)
{
#ifdef NDEBUG
	if (parser->current_token + 1 > parser->token_array->size)
	{
		// TODO: better erorr message
		// TODO: better creation of error tokens.
		const char* error_string
		    = "cannot peek forward, because index is out of range";
		soul_token_t error = {
		    .type   = soul_token_error,
		    .start  = error_string,
		    .length = strlen(error_string),
		};
		return error;
	}
#endif
	return soul_token_array_at(parser->token_array, parser->current_token + 1);
}

static soul_token_t require(soul_parser_t* parser,
                                   soul_token_type_t type)
{
	if (peek(parser).type != type)
	{
		printf("required type: %s, but got %s\n", soul_token_type_to_string(type), soul_token_type_to_string(peek(parser).type));
		// TODO: better creation of error tokens.
		const char* error_string = "required token is missing";
		soul_token_t error       = {
		          .type   = soul_token_error,
		          .start  = error_string,
		          .length = strlen(error_string),
        };
		return error;
	}
	advance(parser);
	return peek_prev(parser);
}
