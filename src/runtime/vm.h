#ifndef SOUL_RUNTIME_VM_H
#define SOUL_RUNTIME_VM_H

#include "runtime/chunk.h"
#include "runtime/value.h"

#include <stdint.h>

typedef struct soul_vm_t soul_vm_t;
struct soul_vm_t
{
	uint32_t ip;
	soul_chunk_t* chunk;
	soul_value_stack_t stack;
};

/** Creates a Soul's virtual machine ready to execute given code chunks. */
soul_vm_t soul_vm_create(void);

void soul_vm_interpret(soul_vm_t* vm, struct soul_chunk_t* chunk);

#endif // SOUL_RUNTIME_VM_H
