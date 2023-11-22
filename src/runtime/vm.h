#ifndef SOUL_RUNTIME_VM_H
#define SOUL_RUNTIME_VM_H

struct soul_chunk_t;
typedef struct soul_vm_t soul_vm_t;

/** Creates a Soul's virtual machine ready to execute given code chunks. */
soul_vm_t* soul_vm_create(void);

/** Destroys given Soul's virtual machine. */
void soul_vm_destroy(soul_vm_t*);

#endif // SOUL_RUNTIME_VM_H
