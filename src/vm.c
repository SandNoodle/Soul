#include "vm.h"

#include <stdio.h> // @TEMP
#include <stdarg.h>
#include <assert.h>

SOUL_STACK_DECLARE(value, soul_value_t)

static void soul__vm_error(soul_vm_t* vm, const char* message, ...)
{
	if(vm->had_panic) return;
	vm->had_panic = true;
	vm->had_error = true;

#if 0 // @TODO CRASHES!
	if(parser->error_callback)
	{
		const soul_token_t token = parser->current_token;
		parser->error_callback("TEST", token.line, message, strlen(message));
	}
#else
	// @TEMP @TODO error_callback!
	va_list varg;
	va_start(varg, message);
	printf("[ERROR] VM: ");
	vprintf(message, varg);
	printf("\n");
	va_end(varg);
#endif
}

//
// Public API
//

SOUL_API void soul_vm_init(soul_vm_t* vm)
{
	vm->ip = 0;

	vm->had_panic = false;
	vm->had_error = false;

	soul__value_stack_new(&vm->stack);
}

SOUL_API void soul_vm_free(soul_vm_t* vm)
{
	soul__value_stack_free(&vm->stack);
}

#define SOUL_BINARY_OP(val_type, as_type, op)               \
	do {                                                    \
		soul_value_t a = soul__value_stack_pop(&vm->stack); \
		soul_value_t b = soul__value_stack_pop(&vm->stack); \
		soul_value_t c = {                                  \
			.type = val_type,                               \
			.as.as_type = a.as.as_type op b.as.as_type,     \
		};                                                  \
		soul__value_stack_push(&vm->stack, c);              \
	} while(0);


SOUL_API soul_result_t soul_vm_interpret(soul_vm_t* vm, soul_chunk_t* chunk)
{
	if(!vm) { return SOUL_COMPILE_ERROR; }
	if(!chunk) { return SOUL_COMPILE_ERROR; }

	vm->ip = 0;

	#define NEXT_BYTE() chunk->code.data[++vm->ip]

	while(vm->ip < chunk->code.size)
	{
		uint8_t opcode = chunk->code.data[vm->ip];
		switch(opcode)
		{
			case OP_NOOP:
				break; // Do nothing.
			case OP_GET_CONST:
				{
					uint8_t index = NEXT_BYTE();
					soul_value_t v = chunk->constants.data[index];
					soul__value_stack_push(&vm->stack, v);
				}
				break;
			case OP_GET_LOCAL:
				{
					uint8_t slot = NEXT_BYTE();
					soul_value_t v = soul__value_stack_peek_at(&vm->stack, slot);
					soul__value_stack_push(&vm->stack, v);
				}
				break;
			case OP_SET_LOCAL:
				{
					uint8_t slot = NEXT_BYTE();
					soul_value_t v = soul__value_stack_peek_at(&vm->stack, slot);
					vm->stack.data[slot] = v;
				}
				break;
			case OP_POP: soul__value_stack_popn(&vm->stack, NEXT_BYTE()); break;
			case OP_ADDI: SOUL_BINARY_OP(SOUL_TYPE_INT,  type_int, +); break;
			case OP_SUBI: SOUL_BINARY_OP(SOUL_TYPE_INT,  type_int, -); break;
			case OP_MULI: SOUL_BINARY_OP(SOUL_TYPE_INT,  type_int, *); break;
			case OP_DIVI: SOUL_BINARY_OP(SOUL_TYPE_INT,  type_int, /); break;
			case OP_ADDF: SOUL_BINARY_OP(SOUL_TYPE_REAL, type_real, +); break;
			case OP_SUBF: SOUL_BINARY_OP(SOUL_TYPE_REAL, type_real, -); break;
			case OP_MULF: SOUL_BINARY_OP(SOUL_TYPE_REAL, type_real, *); break;
			case OP_DIVF: SOUL_BINARY_OP(SOUL_TYPE_REAL, type_real, /); break;
			case OP_PRINT: // @Temp
				{
					soul_value_t a = soul__value_stack_pop(&vm->stack);
					switch(a.type)
					{
						case SOUL_TYPE_INT:
							printf("%lld\n", a.as.type_int);
							break;
						case SOUL_TYPE_REAL:
							printf("%f\n", a.as.type_real);
							break;
						case SOUL_TYPE_BOOL:
							printf("%s\n", a.as.type_bool != 0 ? "true" : "false");
							break;
						default:
							soul__vm_error(vm, "OP_PRINTL Unknown type.");
							break;
					}
				}
				break;
			default:
				soul__vm_error(vm, "Unknown OPCODE: %d.", opcode);
				return SOUL_RUNTIME_ERROR;
		}

		// NOTE: Point at the next opcode.
		vm->ip++;
	}

	return SOUL_SUCCESS;
}

#undef SOUL_BINARY_OP
