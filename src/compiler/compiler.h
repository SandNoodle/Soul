#ifndef SOUL_COMPILER_COMPILER_H
#define SOUL_COMPILER_COMPILER_H

#include <stdint.h>

struct soul_ast_node_t;
struct soul_chunk_t;
typedef struct soul_compiler_t soul_compiler_t;

/** */
soul_compiler_t* soul_compiler_create(void);

/** */
void soul_compiler_destroy(soul_compiler_t* compiler);

struct soul_chunk_t* soul_compiler_compile(soul_compiler_t* compiler, struct soul_ast_node_t* root);

#endif // SOUL_COMPILER_COMPILER_H
