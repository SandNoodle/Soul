#ifndef SOUL_COMPILER_CHUNK_H
#define SOUL_COMPILER_CHUNK_H

#include "allocator.h"
#include "runtime/value.h"

#include <stdint.h>

/**
 * Represents bytecode chunk of instructions that can be exectued by a VM.
 */
typedef struct soul_chunk_t soul_chunk_t;
struct soul_chunk_t
{
	uint8_t* code;
	uint32_t code_size;
	soul_value_array_t constants;
	soul_allocator_t* allocator;
};

/** */
soul_chunk_t soul_chunk_create(soul_allocator_t* allocator);

/** TODO: Temp. Find a better way to do that so we don't have to use stdio.h */
void soul_print_chunk(const soul_chunk_t* chunk);

#endif // SOUL_COMPILER_CHUNK_H
