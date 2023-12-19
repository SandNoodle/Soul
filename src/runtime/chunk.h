#ifndef SOUL_COMPILER_CHUNK_H
#define SOUL_COMPILER_CHUNK_H

#include <stdint.h>

/**
 * Represents bytecode chunk of instructions that can be exectued by a VM.
 */
typedef struct soul_chunk_t soul_chunk_t;
struct soul_chunk_t
{
	uint8_t* code;
	uint32_t code_size;
};

/** */
soul_chunk_t soul_chunk_create(void);

#endif // SOUL_COMPILER_CHUNK_H
