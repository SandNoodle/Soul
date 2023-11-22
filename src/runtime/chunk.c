#include "chunk.h"

#include <stdlib.h>

soul_chunk_t* soul_chunk_create(void)
{
	soul_chunk_t* c = (soul_chunk_t*)malloc(sizeof(soul_chunk_t));
	c->code = NULL;
	c->code_size = 0;
	return c;
}

void soul_chunk_destroy(soul_chunk_t* chunk)
{
	if(!chunk) return;
	free(chunk);
}
