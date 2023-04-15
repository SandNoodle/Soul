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

static void soul__parser_init(soul_parser_t* parser, soul_token_vector_t* array)
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

static bool soul__parser_match(soul_parser_t* p, soul_token_type_t type)
{
	return p->current_token.type == type;
}

static bool soul__parser_is_declaration(soul_token_type_t type)
{
	return type == TOKEN_FN
		|| type == TOKEN_LET
		|| type == TOKEN_DEFINE
		|| type == TOKEN_NATIVE
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
	soul_token_t type_token = soul__parser_require(p, TOKEN_IDENTIFIER);
	soul_ast_identifier_t* type = soul__ast_new_identifier(type_token.start, type_token.length);

	// Consume TOKEN_EQUAL
	soul__parser_require(p, TOKEN_EQUAL);

	// Variable value
	soul_ast_expression_t* value = soul__parser_parse_expression(p);

	return soul__ast_variable_declaration(identifier, type, value, line);
}

static soul_ast_statement_t* soul__parser_parse_body(soul_parser_t* p)
{
	const uint32_t line = p->current_token.line;

	// Require block start(TOKEN_BRACE_LEFT)
	soul__parser_require(p, TOKEN_BRACE_LEFT);

	soul_ast_statement_vector_t* stmts =
		(soul_ast_statement_vector_t*)malloc(sizeof(soul_ast_statement_vector_t));
	soul__new_ast_statement_vector(stmts);
	while(p->current_token.type != TOKEN_BRACE_RIGHT)
	{
		soul_ast_statement_t* s = soul__parser_parse_statement(p);
		soul__ast_statement_vector_push(stmts, s);
	}

	// Require block end(TOKEN_BRACE_RIGHT)
	soul__parser_require(p, TOKEN_BRACE_RIGHT);

	return soul__ast_block_statement(stmts, line);
}

static soul_ast_statement_t* soul__parser_parse_function(soul_parser_t* p)
{
	const uint32_t line = p->current_token.line;

	// Consume function declaration (TOKEN_FN)
	soul__parser_advance(p);

	// Function identifier
	soul_token_t id_token = soul__parser_require(p, TOKEN_IDENTIFIER);
	soul_ast_identifier_t* identifier = soul__ast_new_identifier(id_token.start, id_token.length);

	// (Optional) parameters
	soul_ast_statement_vector_t* params = (soul_ast_statement_vector_t*)malloc(sizeof(soul_ast_statement_vector_t));
	if(soul__parser_match(p, TOKEN_PAREN_LEFT))
	{
		// Consume TOKEN_PAREN_LEFT
		soul__parser_advance(p);

		// NOTE: Parameters is defined as <IDENTIFIER> : <TYPE>
		//       and delimited by ',' (TOKEN_COMMA)
		while(soul__parser_match(p, TOKEN_IDENTIFIER))
		{
			soul__parser_advance(p); // @TODO IDENTIFIER
			soul__parser_require(p, TOKEN_COLON);
			soul__parser_advance(p); // @TODO TYPE

			if(p->current_token.type != TOKEN_COMMA) break;
		}

		soul__parser_require(p, TOKEN_PAREN_RIGHT);
	}

	// Consume type declaration (TOKEN_DOUBLE_COLON);
	soul__parser_require(p, TOKEN_DOUBLE_COLON);

	// Return type
	// @TODO Return type
	soul__parser_advance(p);

	// Function body
	soul_ast_statement_t* body = soul__parser_parse_body(p);

	return soul__ast_function_declaration(identifier, params, body, line);
}

static soul_ast_statement_t* soul__parser_parse_define(soul_parser_t* p)
{
	const uint32_t line = p->current_token.line;

	// Consume define declaration (TOKEN_DEFINE)
	soul__parser_advance(p);

	// Identifier
	soul_token_t id_token = soul__parser_require(p, TOKEN_IDENTIFIER);
	soul_ast_identifier_t* identifier = soul__ast_new_identifier(id_token.start, id_token.length);

	// Expression
	soul_ast_expression_t* value = soul__parser_parse_expression(p);

	return soul__ast_define_declaration(identifier, value, line);
}

static soul_ast_statement_t* soul__parser_parse_if_statement(soul_parser_t* p)
{
	const uint32_t line = p->current_token.line;

	// Consume TOKEN_IF
	soul__parser_advance(p);

	// // Condition
	soul__parser_require(p, TOKEN_PAREN_LEFT);
	soul_ast_expression_t* cond = soul__parser_parse_expression(p);
	soul__parser_require(p, TOKEN_PAREN_RIGHT);

	// Then block
	soul_ast_statement_t* then_stmt = soul__parser_parse_body(p);

	// (Optional) Else
	soul_ast_statement_t* else_stmt = NULL;
	if(soul__parser_match(p, TOKEN_ELSE))
	{
		soul__parser_advance(p);
		else_stmt = soul__parser_parse_body(p);
	}

	return soul__ast_if_statement(cond, then_stmt, else_stmt, line);
}

static soul_ast_statement_t* soul__parser_parse_while_statement(soul_parser_t* p)
{
	const uint32_t line = p->current_token.line;

	// Consume TOKEN_WHILE
	soul__parser_advance(p);

	soul_ast_expression_t* cond = NULL;
	const bool has_params = soul__parser_match(p, TOKEN_PAREN_LEFT);
	if(has_params)
	{
		soul__parser_advance(p); // Skip TOKEN_PAREN_LEFT
		cond = soul__parser_parse_expression(p);
		soul__parser_require(p, TOKEN_PAREN_RIGHT);
	}
	else
	{
		// NOTE: Parameterless variant has form: while { <body> }
		cond = soul__ast_bool_literal_expression(true, line);
		soul__parser_advance(p);
	}

	soul_ast_statement_t* body = soul__parser_parse_body(p);

	return soul__ast_while_statement(cond, body, line);
}
static soul_ast_statement_t* soul__parser_parse_statement(soul_parser_t* p)
{
	switch(p->current_token.type)
	{
		case TOKEN_DEFINE:
			{
				soul_ast_statement_t* s = soul__parser_parse_define(p);
				soul__parser_require(p, TOKEN_SEMICOLON);
				return s;
			}
		case TOKEN_LET:
			{
				soul_ast_statement_t* s = soul__parser_parse_variable(p);
				soul__parser_require(p, TOKEN_SEMICOLON);
				return s;
			}
		case TOKEN_FN:
			return soul__parser_parse_function(p);
		case TOKEN_IF:
			return soul__parser_parse_if_statement(p);
		case TOKEN_WHILE:
			return soul__parser_parse_while_statement(p);
		case TOKEN_BRACE_LEFT:
			return soul__parser_parse_body(p);
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
			{
				// @TODO Currenlty parse as I64.
				soul_value_type_t type = VAL_I64;
				soul_value_t value;
				value.as.i64 = 420; // @TODO
				//
				soul_ast_expression_t* e = soul__ast_number_literal_expression(type, value, token.line);
				soul__parser_advance(p);
				return e;
			}
		case TOKEN_TRUE:
			{
				soul_ast_expression_t* e = soul__ast_bool_literal_expression(true, token.line);
				soul__parser_advance(p);
				return e;
			}
		case TOKEN_FALSE:
			{
				soul_ast_expression_t* e = soul__ast_bool_literal_expression(false, token.line);
				soul__parser_advance(p);
				return e;
			}
		case TOKEN_STRING:
			{
				soul_ast_expression_t* e = soul__ast_string_literal_expression(token.start, token.length, token.line);
				soul__parser_advance(p);
				return e;
			}
		default:
			soul__parser_error(p, "Expected expression.");
			soul__parser_advance(p);
			break;
	}

	// @TODO UNREACHABLE
	return NULL;
}

static soul_ast_t* soul__parser_parse_program(soul_parser_t* p)
{
	/* if(!p->tokens->valid) return soul__invalid_ast; */ // @TODO

	soul_ast_statement_vector_t* stmts = malloc(sizeof(*stmts));
	soul__new_ast_statement_vector(stmts);
	while(p->current_token.type != TOKEN_EOF)
	{
		soul_ast_statement_t* s = soul__parser_parse_statement(p);
		soul__ast_statement_vector_push(stmts, s);
	}

	// @TODO Switch to function scope???
	soul_ast_statement_t* root = soul__ast_block_statement(stmts, 0);

	soul_ast_t* ast = (soul_ast_t*)malloc(sizeof(soul_ast_t));
	ast->valid = true;
	ast->root = root;
	return ast;
}

SOUL_API soul_ast_t* soul_parse(soul_token_vector_t array)
{
	// @TEMP SUPRESS WARNING IN THIS FILE
	SOUL_UNUSED(soul__parser_is_declaration);
	SOUL_UNUSED(soul__parser_parse_function);
	//

	/* if(!array.valid) { return soul__invalid_ast; } */ // @TODO

	soul_parser_t parser;
	soul__parser_init(&parser, &array);

	return soul__parser_parse_program(&parser);
}

SOUL_API void soul_free_ast(soul_ast_t* ast)
{
	ast->valid = false;
	soul__ast_free_statement(ast->root);
	free(ast);
}
