#ifndef SOUL_AST_AST_STRINGIFY_H
#define SOUL_AST_AST_STRINGIFY_H

#include <stdint.h>

struct soul_ast_node_t;

// TODO TEMP: This is a temporary function for debugging.
//            In the future replace it with a function
//            where user can specify buffer to write to.
/** Prints the AST Node into the standard output. */
void soul_ast_node_stringify(struct soul_ast_node_t* node);

#endif // SOUL_AST_AST_STRINGIFY_H
