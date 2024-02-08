#include "chunk.h"

#include <stdlib.h>
#include <stdio.h> // TODO TEMP

soul_chunk_t soul_chunk_create(void)
{
	soul_chunk_t chunk;
	chunk.code      = NULL;
	chunk.code_size = 0;
	return chunk;
}

static void print_instruction(const soul_chunk_t* chunk, size_t* index)
{
	printf("%zu: ", *index);
	switch(chunk->code[*index])
	{
		default:
			index++;
			break;
	}
	printf("\n");
}

void soul_print_chunk(const soul_chunk_t* chunk)
{
	if(!chunk || !chunk->code) return;

	for(size_t index = 0; index < chunk->code_size; ++index)
	{
		print_instruction(chunk, &index);
	}
}
