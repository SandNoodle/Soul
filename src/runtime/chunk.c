#include "chunk.h"

#include <stdlib.h>

soul_chunk_t soul_chunk_create(void)
{
	soul_chunk_t chunk;
	chunk.code      = NULL;
	chunk.code_size = 0;
	return chunk;
}
