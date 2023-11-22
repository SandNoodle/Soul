#ifndef SOUL_PARSER_PARSER_H
#define SOUL_PARSER_PARSER_H

#include <stdint.h>

struct soul_token_array_t;
struct soul_ast_node_t;
typedef struct soul_parser_t soul_parser_t;

/** */
soul_parser_t* soul_parser_create(void);

/** */
void soul_parser_destroy(soul_parser_t* parser);

/**
 * @brief
 *
 * @param
 * @param
 * @param
 * @return
 */
struct soul_ast_node_t* soul_parser_parse(soul_parser_t* parser, struct soul_token_array_t* tokens);

#endif // SOUL_PARSER_PARSER_H
