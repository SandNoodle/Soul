#include "vm.h"

#include "opcode.h"
#include "chunk.h"

#define READ_BYTE() chunk->code[++vm->ip]

soul_vm_t soul_vm_create(void)
{
	soul_vm_t vm;
	vm.ip = 0;
	vm.sp = 0;
	vm.code = NULL;
	vm.stack = NULL;
	return vm;
}

void soul_vm_interpret(soul_vm_t* vm, soul_chunk_t* chunk)
{
	for (;;)
	{
		int8_t current_op = READ_BYTE();
		switch (current_op)
		{
			case soul_op_noop:
				READ_BYTE();
				break;
		}
	}
}
