#ifndef SOUL_PARSER_PARSER_H
#define SOUL_PARSER_PARSER_H

#include "ast/ast.h"
#include "lexer/token.h"

typedef struct soul_parser_t soul_parser_t;
struct soul_parser_t
{
	soul_token_array_t* token_array;
	size_t current_token;

	bool had_panic;
	bool had_error;
};

/** Creates parser ready for parsing. */
soul_parser_t soul_parser_create(void);

/**
 * @brief Parses given token array into its AST representation.
 * @param parser parser to use.
 * @param tokens tokens to parse.
 * @return Abstract Syntax Tree representation of a token array.
 */
soul_ast_node_t* soul_parser_parse(soul_parser_t* parser,
                                   soul_token_array_t* tokens);

#endif // SOUL_PARSER_PARSER_H
