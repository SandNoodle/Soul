#include "vm.h"

#include "runtime/opcode.h"
#include <stdio.h> // TEMP

#define READ_BYTE() vm->chunk->code[vm->ip++]
#define READ_SHORT() READ_BYTE() << 8 | READ_BYTE()

#define SOUL_TRUE 1
#define SOUL_FALSE 0

#define BINARY_OP(op, as_type)                                                                   \
	const soul_value_t a = soul_value_stack_pop(&vm->stack);                                     \
	const soul_value_t b = soul_value_stack_pop(&vm->stack);                                     \
	soul_value_stack_push(&vm->stack, (soul_value_t){.as_type = a.as_type op b.as_type});

static void reset(soul_vm_t*);

soul_vm_t soul_vm_create(void)
{
	soul_vm_t vm;
	vm.ip = 0;
	vm.chunk = NULL;
	vm.stack = soul_value_stack_create();
	return vm;
}

void soul_vm_interpret(soul_vm_t* vm, soul_chunk_t* chunk)
{
	reset(vm);
	vm->chunk = chunk;
	bool should_stop = false;
	while(!should_stop)
	{
		uint8_t current_op = READ_BYTE();
		switch (current_op)
		{
			case soul_op_noop: {
				// Do nothing.
				break;
			}
			case soul_op_pop: {
				soul_value_stack_pop(&vm->stack);
				break;
			}
			case soul_op_pop_n: {
				uint8_t count = READ_BYTE();
				soul_value_stack_pop_n(&vm->stack, count);
			}
			case soul_op_get_local: {
				// TODO
				break;
			}
			case soul_op_set_local: {
				// TODO:
				break;
			}
			case soul_op_push_const: {
				const uint16_t index = READ_SHORT();
				const soul_value_t value = vm->chunk->constants.values[index];
				soul_value_stack_push(&vm->stack, value);
				break;
			}
			case soul_op_push_true: {
				soul_value_stack_push(&vm->stack, (soul_value_t){.int_value = SOUL_TRUE});
				break;
			}
			case soul_op_push_false: {
				soul_value_stack_push(&vm->stack, (soul_value_t){.int_value = SOUL_FALSE});
				break;
			}
			case soul_op_jump: {
				const int16_t offset = READ_SHORT();
				vm->ip += offset;
				break;
			}
			case soul_op_jump_false: {
				const soul_value_t value = soul_value_stack_pop(&vm->stack);
				if(value.int_value == SOUL_FALSE)
				{
					vm->ip += READ_SHORT();
				}
				break;
			}
			case soul_op_jump_true: {
				const soul_value_t value = soul_value_stack_pop(&vm->stack);
				if(value.int_value == SOUL_TRUE)
				{
					vm->ip += READ_SHORT();
				}
				break;
			}
			case soul_op_addi: { BINARY_OP(+, int_value); break; }
			case soul_op_subi: { BINARY_OP(-, int_value); break; }
			case soul_op_muli: { BINARY_OP(*, int_value); break; }
			case soul_op_divi: { BINARY_OP(/, int_value); break; }
			case soul_op_addf: { BINARY_OP(+, float_value); break; }
			case soul_op_subf: { BINARY_OP(-, float_value); break; }
			case soul_op_mulf: { BINARY_OP(*, float_value); break; }
			case soul_op_divf: { BINARY_OP(/, float_value); break; }
			case soul_op_printi: {
				const soul_value_t value = soul_value_stack_peek(&vm->stack);
				printf("%lld\n", value.int_value); fflush(stdout);
				break;
			}
			case soul_op_halt:
			default:
				should_stop = true;
				break;
		}
		if(should_stop)
			break;
	}
}

//
// Private
//

static void reset(soul_vm_t* vm)
{
	if(!vm) return;
	vm->ip = 0;
	vm->chunk = NULL;
	soul_value_stack_reset(&vm->stack);
}
