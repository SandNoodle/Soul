#include "compiler.h"

#include "soul_config.h"
#include "ast/ast.h"
#include "runtime/chunk.h"
#include "runtime/opcode.h"

#include <stdlib.h>

struct soul_compiler_t
{
};

soul_compiler_t* soul_compiler_create(void)
{
	soul_compiler_t* c = (soul_compiler_t*)malloc(sizeof(soul_compiler_t));

	return c;
}

void soul_compiler_destroy(soul_compiler_t* compiler)
{
	if(!compiler) return;
	free(compiler);
}

soul_chunk_t* soul_compiler_compile(soul_compiler_t* compiler, soul_ast_node_t* root)
{
	if(!compiler || !root) return NULL;

	soul_chunk_t* chunk = soul_chunk_create();

	return chunk;
}
