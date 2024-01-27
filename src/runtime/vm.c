#include "vm.h"

#include "opcode.h"
#include "chunk.h"

soul_vm_t soul_vm_create(void)
{
	soul_vm_t vm;
	vm.ip = 0;
	return vm;
}

void soul_vm_interpret(soul_vm_t* vm, soul_chunk_t* chunk)
{
	for (;;)
	{
		switch (chunk->code[vm->ip])
		{
			case soul_op_noop:
				vm->ip++;
				break;
			case soul_op_pop:
				vm->ip++;
		}
	}
}
