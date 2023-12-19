#ifndef SOUL_RUNTIME_VM_H
#define SOUL_RUNTIME_VM_H

#include <stdint.h>

struct soul_chunk_t;
typedef struct soul_vm_t soul_vm_t;
struct soul_vm_t
{
	uint32_t ip;
};

/** Creates a Soul's virtual machine ready to execute given code chunks. */
soul_vm_t soul_vm_create(void);

#endif // SOUL_RUNTIME_VM_H
