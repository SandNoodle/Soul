#include "parser.h"

#include <stdio.h>

#include "token.h"

// @TEMP @DEBUG
#define DEBUG() printf("[DEBUG] %s:%d: %s\n", __FILE__, __LINE__, __func__); fflush(stdout);

// Forward declare
static soul_ast_expression_t* soul__parser_parse_expression(soul_parser_t* p);
static soul_ast_statement_t* soul__parser_parse_statement(soul_parser_t* p);

static const soul_ast_t soul__invalid_ast = {
	.root = NULL,
	.valid = false,
};

static void soul__parser_init(soul_parser_t* parser, soul_token_array_t* array)
{
	parser->had_panic = false;
	parser->had_error = false;
	parser->tokens = array;
	parser->tokens_index = 0;
	parser->current_token = parser->tokens->data[0];
}

static void soul__parser_error(soul_parser_t* parser, const char* message)
{
	if(parser->had_panic) return;
	parser->had_panic = true;
	parser->had_error = true;

#if 0 // @TODO CRASHES!
	if(parser->error_callback)
	{
		const soul_token_t token = parser->current_token;
		parser->error_callback("TEST", token.line, message, strlen(message));
	}
#else
	printf("[ERROR] Parsing failed: %s\n", message);
#endif
}

static void soul__parser_advance(soul_parser_t* p)
{
	if(p->tokens_index + 1 >= p->tokens->size)
	{
		soul__parser_error(p, "Parser cannot advance further into token array.");
		return;
	}

	p->tokens_index++;
	p->current_token = p->tokens->data[p->tokens_index];

	if(p->current_token.type == TOKEN_ERROR)
	{
		soul__parser_error(p, "Invalid token.");
	}
};

static soul_token_t soul__parser_require(soul_parser_t* p, soul_token_type_t type)
{
	if(p->current_token.type == type)
	{
		soul_token_t current_token = p->current_token;
		soul__parser_advance(p);
		return current_token;
	}

	soul__parser_error(p, "Expected token type: '%s', but found: '%s"); // @TODO Format

	return (soul_token_t){TOKEN_ERROR, NULL, 0, 0}; // @TODO Line
}

static bool soul__parser_is_declaration(soul_token_type_t type)
{
	return type == TOKEN_FN
		|| type == TOKEN_LET
		|| type == TOKEN_STRUCT
		|| type == TOKEN_ENUM;
}

//
// Statements
//

static soul_ast_statement_t* soul__parser_parse_variable(soul_parser_t* p)
{
	const uint32_t line = p->current_token.line;

	// Consume variable declaration (TOKEN_LET)
	soul__parser_advance(p);

	// Variable identifier
	soul_token_t id_token = soul__parser_require(p, TOKEN_IDENTIFIER);
	soul_ast_identifier_t* identifier = soul__ast_new_identifier(id_token.start, id_token.length);

	// Consume type declaration (TOKEN_COLON)
	soul__parser_require(p, TOKEN_COLON);

	// Token Type
	/* soul_token_t type_token = p->current_token; */
	soul__parser_advance(p); // @TODO: CHECK TYPE!

	// Consume TOKEN_EQUAL
	soul__parser_require(p, TOKEN_EQUAL);

	// Variable value
	// @TODO Currently only numbers are supported.
	soul_ast_expression_t* value = soul__parser_parse_expression(p);

	return soul__ast_variable_declaration(identifier, value, line);
}

static soul_ast_statement_t* soul__parser_parse_function(soul_parser_t* p)
{
	SOUL_UNUSED(p);
	return NULL; // @TEMP @nocheckin
}

static soul_ast_statement_t* soul__parser_parse_statement(soul_parser_t* p)
{
	switch(p->current_token.type)
	{
		case TOKEN_LET:
			soul_ast_statement_t* s = soul__parser_parse_variable(p);
			soul__parser_require(p, TOKEN_SEMICOLON);
			return s;
		case TOKEN_BRACE_LEFT:
			soul__parser_require(p, TOKEN_BRACE_LEFT); // Begin scope
			// @TODO
			soul__parser_require(p, TOKEN_BRACE_RIGHT); // End scope
			break;
		default:
			soul__parser_error(p, "Expected statement.");
			soul__parser_advance(p);
			break;
	}

	// @TODO UNREACHABLE
	return NULL;
}

//
// Expressions
//

static soul_ast_expression_t* soul__parser_parse_expression(soul_parser_t* p)
{
	soul_token_t token = p->current_token;
	switch(token.type)
	{
		case TOKEN_NUMBER:
			// @TODO: TYPES!
			// @TODO: VALUES!
			soul_value_type_t type = VAL_I64;
			soul_value_t value;
			value.as.i64 = 420;
			//
			soul_ast_expression_t* e = soul__ast_number_literal_expression(type, value, token.line);
			soul__parser_advance(p);
			return e;
		default:
			soul__parser_error(p, "Expected expression.");
			soul__parser_advance(p);
			break;
	}

	// @TODO UNREACHABLE
	return NULL;
}

static soul_ast_t soul__parser_parse_program(soul_parser_t* p)
{
	if(!p->tokens->valid) return soul__invalid_ast;

	// @TODO
	while(p->current_token.type != TOKEN_EOF)
	{
		soul_ast_statement_t* s = soul__parser_parse_statement(p);
		soul__ast_print_statement(s);
		soul__ast_free_statement(s);
	}

	return soul__invalid_ast; // @TEMP
}

SOUL_API soul_ast_t soul_parse(soul_token_array_t array)
{
	// @TEMP SUPRESS WARNING IN THIS FILE
	SOUL_UNUSED(soul__parser_is_declaration);
	SOUL_UNUSED(soul__parser_parse_function);
	//

	if(!array.valid) { return soul__invalid_ast; }

	soul_parser_t parser;
	soul__parser_init(&parser, &array);

	return soul__parser_parse_program(&parser);
}
