#ifndef SOUL_RUNTIME_VM_H
#define SOUL_RUNTIME_VM_H

#include <stdint.h>

struct soul_chunk_t;
union soul_value_t;
typedef struct soul_vm_t soul_vm_t;
struct soul_vm_t
{
	uint32_t ip; // Instruction pointer.
	uint32_t sp; // Stack pointer.
	struct soul_chunk_t* code;
	union soul_value_t* stack;
};

/** Creates a Soul's virtual machine ready to execute given code chunks. */
soul_vm_t soul_vm_create(void);

void soul_vm_interpret(soul_vm_t* vm, struct soul_chunk_t* chunk);

#endif // SOUL_RUNTIME_VM_H
