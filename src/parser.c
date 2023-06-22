#include "parser.h"

#include <stdio.h> // @TEMP
#include <stdarg.h>

#include "token.h"

// @TEMP @DEBUG
#define DEBUG_HERE() printf("[DEBUG] %s:%d\n", __func__, __LINE__); fflush(stdout);
#define DEBUG_TOKEN(x) printf("[DEBUG] TOKEN: '%s'\n", soul_token_to_string((x).type)); fflush(stdout);

// Forward declare
static soul_ast_expression_t* soul__parser_parse_expression(soul_parser_t*);
static soul_ast_statement_t* soul__parser_parse_statement(soul_parser_t*);
static soul_ast_expression_t* soul__parser_parse_precedence_expression(soul_parser_t*, soul_precedence_t);
static soul_precedence_rule_t soul__parser_get_precedence_rule(soul_token_type_t);

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

static void soul__parser_error(soul_parser_t* parser, const char* message, ...)
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
	// @TEMP @TODO error_callback!
	va_list varg;
	va_start(varg, message);
	printf("[ERROR] PARSER: ");
	vprintf(message, varg);
	printf("\n");
	va_end(varg);
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
}

static soul_token_t soul__parser_require(soul_parser_t* p, soul_token_type_t type)
{
	if(p->current_token.type == type)
	{
		soul_token_t current_token = p->current_token;
		soul__parser_advance(p);
		return current_token;
	}

	soul__parser_error(p, "Expected token type: '%s', but found: '%s'",
		soul_token_to_string(type), soul_token_to_string(p->current_token.type));

	return (soul_token_t){TOKEN_ERROR, NULL, 0, p->current_token.line};
}

#if 0
static soul_token_t soul__parser_require_any(soul_parser_t* p, soul_token_type_t* types, size_t length)
{
	for(size_t i = 0; i < length; ++i)
	{
		if(p->current_token.type == types[i])
		{
			soul_token_t current_token = p->current_token;
			soul__parser_advance(p);
			return current_token;
		}
	}

	// @TODO: LIST TOKENS
	soul__parser_error(p, "Expected tokens were not found.");
	return (soul_token_t){TOKEN_ERROR, NULL, 0, p->current_token.line};
}
#endif

static bool soul__parser_match(soul_parser_t* p, soul_token_type_t type)
{
	return p->current_token.type == type;
}

#if 0
static bool soul__parser_match_any(soul_parser_t* p, soul_token_type_t* types, size_t length)
{
	for(size_t i = 0; i < length; ++i)
	{
		if(soul__parser_match(p, types[i])) return true;
	}
	return false;
}
#endif

#if 0
static soul_token_t soul__parser_peek(soul_parser_t* p)
{
	return p->tokens->data[p->tokens_index];
}
#endif

static soul_token_t soul__parser_peek_next(soul_parser_t* p)
{
	if(p->tokens_index + 1 > p->tokens->size)
	{
		soul__parser_error(p, "Cannot peek the next token - parser is at the end of token list.");
		return p->tokens->data[p->tokens->size - 1];
	}

	return p->tokens->data[p->tokens_index + 1];
}
static soul_token_t soul__parser_peek_prev(soul_parser_t* p)
{
	if(p->tokens_index == 0)
	{
		soul__parser_error(p, "Cannot peek the previous token - parser is at the start of token list.");
		return p->tokens->data[0];
	}

	return p->tokens->data[p->tokens_index - 1];
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

static bool soul__parser_is_assigment(soul_token_type_t type)
{
	return type == TOKEN_EQUAL
		|| type == TOKEN_PLUS_EQUAL
		|| type == TOKEN_MINUS_EQUAL
		|| type == TOKEN_STAR_EQUAL
		|| type == TOKEN_SLASH_EQUAL;
}

#if 0
static bool soul__parser_is_compound_assigment(soul_token_type_t type)
{
	return type != TOKEN_EQUAL && soul__parser_is_assigment(type);
}
#endif

static bool soul__parser_is_literal(soul_token_type_t type)
{
	return type == TOKEN_IDENTIFIER
		|| type == TOKEN_NUMBER
		|| type == TOKEN_TRUE
		|| type == TOKEN_FALSE
		|| type == TOKEN_STRING;
}

static bool soul__parser_is_synchronization(soul_token_type_t type)
{
	return type == TOKEN_FN
		|| type == TOKEN_LET
		|| type == TOKEN_IF
		|| type == TOKEN_FOR
		|| type == TOKEN_WHILE
		|| type == TOKEN_RETURN
		|| type == TOKEN_STRUCT
		|| type == TOKEN_ENUM
		|| type == TOKEN_BRACE_LEFT; // Blocks
}

static void soul__parser_synchronize(soul_parser_t* p)
{
	p->had_panic = false;

	while(!soul__parser_match(p, TOKEN_EOF))
	{
		if(!soul__parser_is_synchronization(p->current_token.type))
		{
			soul__parser_advance(p);
		}

		break; // Synchronized!
	}
}

//
// Statements
//

static soul_ast_statement_t* soul__parser_parse_variable(soul_parser_t* p)
{
	const uint32_t line = p->current_token.line;

	// Consume variable declaration (TOKEN_LET)
	soul__parser_advance(p);

	// (Optional) Mutable variable
	bool is_mut = false;
	if(p->current_token.type == TOKEN_MUT)
	{
		is_mut = true;
		soul__parser_advance(p);
	}

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

	return soul__ast_variable_declaration(identifier, type, value, is_mut, line);
}

static soul_ast_statement_t* soul__parser_parse_body(soul_parser_t* p)
{
	const uint32_t line = p->current_token.line;

	// Require block start(TOKEN_BRACE_LEFT)
	soul__parser_require(p, TOKEN_BRACE_LEFT);

	soul_ast_statement_vector_t* stmts =
		(soul_ast_statement_vector_t*)malloc(sizeof(soul_ast_statement_vector_t));
	soul__ast_statement_vector_new(stmts);
	while(p->current_token.type != TOKEN_BRACE_RIGHT)
	{
		soul_ast_statement_t* s = soul__parser_parse_statement(p);
		soul__ast_statement_vector_push(stmts, s);

		// NOTE: Prevent trying to parse unterminated blocks.
		if(p->current_token.type == TOKEN_EOF) break;
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
	soul_token_t return_token = soul__parser_require(p, TOKEN_IDENTIFIER);
	soul_ast_identifier_t* return_type = soul__ast_new_identifier(return_token.start, return_token.length);

	// Function body
	soul_ast_statement_t* body = soul__parser_parse_body(p);

	return soul__ast_function_declaration(identifier, params, return_type, body, line);
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

	// Condition
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

static soul_ast_statement_t* soul__parser_parse_for_statement(soul_parser_t* p)
{
	const uint32_t line = p->current_token.line;

	// Consume TOKEN_FOR
	soul__parser_advance(p);

	soul__parser_require(p, TOKEN_PAREN_LEFT);

	// Mutable variable declaration
	// @TODO Currently implemented just as variable declaration,
	//       however in the future I'd like to remove 'let mut' prefix
	//       for a cleaner <identifier> : <type> sytnax.
	soul_ast_statement_t* var = NULL;
	if(!soul__parser_match(p, TOKEN_SEMICOLON))
	{
		var = soul__parser_parse_variable(p);
	}

	soul__parser_require(p, TOKEN_SEMICOLON);

	// Condition
	soul_ast_expression_t* cond = NULL;
	if(!soul__parser_match(p, TOKEN_SEMICOLON))
	{
		cond = soul__parser_parse_expression(p);
	}

	soul__parser_require(p, TOKEN_SEMICOLON);

	// Increment
	soul_ast_expression_t* actual = NULL;
	if(!soul__parser_match(p, TOKEN_PAREN_RIGHT))
	{
		actual = soul__parser_parse_expression(p);
	}

	soul__parser_require(p, TOKEN_PAREN_RIGHT);

	// Body
	soul_ast_statement_t* body = soul__parser_parse_body(p);

	return soul__ast_for_statement(var, cond, actual, body, line);
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

// @TODO
#if 0
static soul_ast_statement_t* soul__parser_parse_compound_assigment_expr_statement(soul_parser_t* p)
{
	const uint32_t line = p->current_token.line;

	soul__parser_advance(p);

	// Consume assigment operator
	soul__parser_advance(p);

	// Parse RVALUE.
	soul_ast_expression_t* right = soul__parser_parse_expression(p);

	return soul__ast_expression_statement(NULL, line); // @TODO
}
#endif

static soul_ast_statement_t* soul__parser_parse_assigment_expr_statement(soul_parser_t* p)
{
	const uint32_t line = p->current_token.line;

	// @TODO get what type of expression we are looking at - CURRENTLY HARDCODED
	soul_ast_expression_t* left = soul__parser_parse_expression(p);

	// Consume assigment operator
	soul__parser_advance(p);

	// Parse RVALUE.
	soul_ast_expression_t* right = soul__parser_parse_expression(p);

	soul_ast_expression_t* expr = soul__ast_assign_expression(left, right, line);

	return soul__ast_expression_statement(expr, line);
}

static soul_ast_statement_t* soul__parser_parse_expression_statement(soul_parser_t* p)
{
	soul_token_type_t next_type = soul__parser_peek_next(p).type;

#if 0
	if(soul__parser_is_compound_assigment(next_type))
		return soul__parser_parse_compound_assigment_expr_statement(p);
#endif

	if(soul__parser_is_assigment(next_type))
		return soul__parser_parse_assigment_expr_statement(p);

	soul_ast_expression_t* e = soul__parser_parse_expression(p);
	return soul__ast_expression_statement(e, p->current_token.line);
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
		case TOKEN_FOR:
			return soul__parser_parse_for_statement(p);
		case TOKEN_WHILE:
			return soul__parser_parse_while_statement(p);
		case TOKEN_BRACE_LEFT:
			return soul__parser_parse_body(p);
		default:
			break;
	}

	soul_ast_statement_t* statement = soul__parser_parse_expression_statement(p);
	soul__parser_require(p, TOKEN_SEMICOLON);
	return statement;
}

//
// Expressions
//

static soul_value_t soul__parser_parse_number(soul_parser_t* p)
{
	soul_token_t token = soul__parser_peek_prev(p);
	soul_value_t value;
#if 0
	// Peek backwards to figure out type declared.
	soul_token_t decl_type = p->tokens->data[p->tokens_index - 2];

	switch(soul__parser_token_to_value_type(decl_type))
	{
		case : value.type = SOUL_VAL_U8;  value.as.u8  = (uint8_t)strtoull(token.start, NULL, 0); return value;
		case : value.type = SOUL_VAL_U16; value.as.u16 = (uint16_t)strtoull(token.start, NULL, 0); return value;
		case : value.type = SOUL_VAL_U32; value.as.u32 = (uint32_t)strtoull(token.start, NULL, 0); return value;
		case : value.type = SOUL_VAL_U64; value.as.u64 = (uint64_t)strtoull(token.start, NULL, 0); return value;
		case : value.type = SOUL_VAL_I8;  value.as.i8  = (int8_t)strtoll(token.start, NULL, 0); return value;
		case : value.type = SOUL_VAL_I16; value.as.i16 = (int16_t)strtoll(token.start, NULL, 0); return value;
		case : value.type = SOUL_VAL_I32; value.as.i32 = (int32_t)strtoll(token.start, NULL, 0); return value;
		case : value.type = SOUL_VAL_I64; value.as.i64 = (int64_t)strtoll(token.start, NULL, 0); return value;
		case : value.type = SOUL_VAL_F32; value.as.f32 = strtof(token.start, NULL); return value;
		case : value.type = SOUL_VAL_F64; value.as.f64 = strtod(token.start, NULL); return value;
		default:
			break;
	}

	soul__parser_error(p, "Expected numeric primitive type, but got: '%s'",
		soul_token_to_string(p->current_token.type));
	value.type = VAL_U64; value.as.u64 = 0;
#endif

	// @TODO Temp
	value.type = SOUL_TYPE_INT;
	value.as.type_int = (int64_t)strtoll(token.start, NULL, 0);

	return value;
}

static soul_ast_expression_t* soul__parser_parse_literal(soul_parser_t* p)
{
	soul_token_t token = soul__parser_peek_prev(p);
	switch(token.type)
	{
		case TOKEN_IDENTIFIER:
			{
				return soul__ast_variable_literal_expression(token.start, token.length, token.line);
			}
		case TOKEN_NUMBER:
			{
				soul_value_t v = soul__parser_parse_number(p);
				return soul__ast_number_literal_expression(v, token.line);
			}
		case TOKEN_TRUE:
			{
				return  soul__ast_bool_literal_expression(true, token.line);
			}
		case TOKEN_FALSE:
			{
				return soul__ast_bool_literal_expression(false, token.line);
			}
		case TOKEN_STRING:
			{
				return soul__ast_string_literal_expression(token.start + 1, token.length - 2, token.line);
			}
		default:
			soul__parser_error(p, "Expected literal token, but got '%s'.", soul_token_to_string(token.type));
			break;
	}

	// NOTE: UNREACHABLE due to language not supporting NULL values. @TODO?
	return NULL;
}

static soul_ast_expression_t* soul__parser_parse_binary_expression(
	soul_parser_t* p, soul_ast_expression_t* l)
{
	soul_token_t op = soul__parser_peek_prev(p);
	soul_precedence_rule_t rule = soul__parser_get_precedence_rule(op.type);
	soul_ast_expression_t* r = soul__parser_parse_precedence_expression(p, (soul_precedence_t)(rule.precedence + 1));

	return soul__ast_binary_expression(l, r, op.type, op.line);
}

static soul_ast_expression_t* soul__parser_parse_unary_expression(soul_parser_t* p)
{
	soul_token_t op = soul__parser_peek_prev(p);
	soul__parser_advance(p);
	soul_ast_expression_t* l = soul__parser_parse_literal(p);
	return soul__ast_unary_expression(l, op.type, op.line);
}

static soul_precedence_rule_t soul__parser_get_precedence_rule(soul_token_type_t type)
{
	if(soul__parser_is_literal(type))
		return (soul_precedence_rule_t) { soul__parser_parse_literal, NULL, SOUL_PREC_NONE };

	switch(type)
	{
		case TOKEN_MINUS:
			return (soul_precedence_rule_t) {
				soul__parser_parse_unary_expression,
				soul__parser_parse_binary_expression,
				SOUL_PREC_ADDITIVE };
		case TOKEN_PLUS:
			return (soul_precedence_rule_t) { NULL, soul__parser_parse_binary_expression, SOUL_PREC_ADDITIVE };
		case TOKEN_STAR:
		case TOKEN_SLASH:
			return (soul_precedence_rule_t) { NULL, soul__parser_parse_binary_expression, SOUL_PREC_MULTIPLICATIVE };
		default:
			break;
	}

	// NOTE: No Precedence.
	return (soul_precedence_rule_t) { NULL, NULL, SOUL_PREC_NONE };
}

static soul_ast_expression_t* soul__parser_parse_precedence_expression(
	soul_parser_t* p, soul_precedence_t precedence)
{
	soul__parser_advance(p);
	soul_prefix_precedence_fn prefix_rule = soul__parser_get_precedence_rule(soul__parser_peek_prev(p).type).prefix;
	if(!prefix_rule)
	{
		soul__parser_error(p, "Expected prefix expression.");
		return NULL;
	}
	soul_ast_expression_t* prefix_expr = prefix_rule(p);

	while(precedence <= soul__parser_get_precedence_rule(p->current_token.type).precedence)
	{
		soul__parser_advance(p);
		soul_infix_precedence_fn infix_rule = soul__parser_get_precedence_rule(soul__parser_peek_prev(p).type).infix;
		prefix_expr = infix_rule(p, prefix_expr);
	}

	return prefix_expr;
}

static soul_ast_expression_t* soul__parser_parse_expression(soul_parser_t* p)
{
	// NOTE: Starting precedence has to be at least 1 higher than no precedence.
	return soul__parser_parse_precedence_expression(p, (soul_precedence_t)(SOUL_PREC_NONE + 1));
}

static soul_ast_t* soul__parser_parse_program(soul_parser_t* p)
{
	/* if(!p->tokens->valid) return soul__invalid_ast; */ // @TODO

	soul_ast_statement_vector_t* stmts = (soul_ast_statement_vector_t*)malloc(sizeof(soul_ast_statement_vector_t));
	soul__ast_statement_vector_new(stmts);
	while(p->current_token.type != TOKEN_EOF)
	{
		soul_ast_statement_t* s = soul__parser_parse_statement(p);
		soul__ast_statement_vector_push(stmts, s);

		if(p->had_panic)
		{
			soul__parser_synchronize(p);
		}
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
