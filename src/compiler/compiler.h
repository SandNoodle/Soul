#ifndef SOUL_COMPILER_COMPILER_H
#define SOUL_COMPILER_COMPILER_H

#include "allocator.h"
#include "ast/ast.h"
#include "runtime/chunk.h"

typedef struct soul_compiler_t soul_compiler_t;
struct soul_compiler_t
{
	soul_chunk_t* chunk;
	uint32_t current_depth;

	bool had_error;
	bool had_panic;

	soul_allocator_t* allocator;
};

/** Creates compiler ready for compiling AST into a VM's bytecode. */
soul_compiler_t soul_compiler_create(soul_allocator_t* allocator);

soul_chunk_t soul_compiler_compile(soul_compiler_t* compiler,
                                   soul_ast_node_t* root);

#endif // SOUL_COMPILER_COMPILER_H
