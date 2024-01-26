#ifndef SOUL_PARSER_PARSER_H
#define SOUL_PARSER_PARSER_H

#include <stdbool.h>
#include <stdint.h>

struct soul_token_array_t;
struct soul_ast_node_t;

typedef struct soul_parser_t soul_parser_t;
struct soul_parser_t
{
	struct soul_token_array_t* token_array;
	size_t current_token;

	bool had_panic;
	bool had_error;
};

/** Creates parser ready for parsing. */
soul_parser_t soul_parser_create(void);

/**
 * @brief
 *
 * @param
 * @param
 * @param
 * @return
 */
struct soul_ast_node_t* soul_parser_parse(soul_parser_t* parser,
                                          struct soul_token_array_t* tokens);

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
typedef struct soul_ast_node_t* (*soul_infix_precedence_fn)(
    soul_parser_t*, struct soul_ast_node_t*);

typedef struct soul_precedence_rule_t soul_precedence_rule_t;
struct soul_precedence_rule_t
{
	soul_precedence_t precedence;
	soul_prefix_precedence_fn prefix;
	soul_infix_precedence_fn infix;
};

#endif // SOUL_PARSER_PARSER_H
