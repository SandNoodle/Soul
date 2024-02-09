#ifndef SOUL_COMPILER_COMPILER_H
#define SOUL_COMPILER_COMPILER_H

#include "ast/ast.h"
#include "runtime/chunk.h"

typedef struct soul_compiler_t soul_compiler_t;
struct soul_compiler_t
{
	soul_chunk_t* chunk;
};

/** Creates compiler ready for compiling AST into a VM's bytecode. */
soul_compiler_t soul_compiler_create(void);

soul_chunk_t soul_compiler_compile(soul_compiler_t* compiler,
                                   soul_ast_node_t* root);

#endif // SOUL_COMPILER_COMPILER_H
