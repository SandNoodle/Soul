#pragma once

#include "soul.h"

#include "token.h"
#include "ast.h"

typedef struct
{
	soul_token_vector_t* tokens;
	size_t tokens_index;

	soul_token_t current_token;

	bool had_panic;
	bool had_error;

	soul_message_callback_t error_callback;
	soul_message_callback_t warn_callback;
} soul_parser_t;

struct soul_parser_config_t {
	soul_message_callback_t warn_callback;  // Can be null.
	soul_message_callback_t error_callback; // Can be null.
};

typedef enum {
	SOUL_PREC_NONE,
	SOUL_PREC_ASSIGN,         // =
	SOUL_PREC_OR,             // ||
	SOUL_PREC_AND,            // &&
	SOUL_PREC_EQUAL,          // == !=
	SOUL_PREC_COMPARE,        // < > <= =>
	SOUL_PREC_ADDITIVE,       // + -
	SOUL_PREC_MULTIPLICATIVE, // * /
	SOUL_PREC_UNARY,          // ! -
	SOUL_PREC_CALL,           // @TODO
	SOUL_PREC_PRIMARY,
} soul_precedence_t;

typedef soul_ast_expression_t* (*soul_prefix_precedence_fn)(soul_parser_t*);
typedef soul_ast_expression_t* (*soul_infix_precedence_fn)(soul_parser_t*, soul_ast_expression_t* l);

typedef struct {
	soul_prefix_precedence_fn prefix;
	soul_infix_precedence_fn infix;
	soul_precedence_t precedence;
} soul_precedence_rule_t;
