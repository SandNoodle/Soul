#include "vm.h"

#include <assert.h>

static void soul__vm_stack_init(soul_stack_t* stack)
{
	stack->data = (soul_value_t*)malloc(VM_STACK_MIN_SIZE * sizeof(soul_value_t));
	stack->capacity = VM_STACK_MIN_SIZE;
	stack->size = 0;
}

static void soul__vm_stack_free(soul_stack_t* stack)
{
	free(stack->data);
	stack->capacity = 0;
	stack->size = 0;
}

static void soul__vm_stack_grow(soul_stack_t* stack)
{
	uint32_t new_capacity = stack->capacity * 2.0f;
	stack->data = (soul_value_t*)realloc(stack->data, new_capacity);
	stack->capacity = new_capacity;
}

static soul_value_t soul__vm_stack_peek(soul_stack_t* stack)
{
	return stack->data[stack->size];
}

static void soul__vm_stack_push(soul_stack_t* stack, soul_value_t value)
{
	if(stack->size == stack->capacity - 1)
	{
		soul__vm_stack_grow(stack);
	}

	stack->size++;
	stack->data[stack->size] = value;
}

static soul_value_t soul__vm_stack_pop(soul_stack_t* stack)
{
	assert(stack->size == 0 && "VM's stack underflow.");

	soul_value_t value = stack->data[stack->size];
	stack->size--;

	return value;
}

SOUL_API void soul_vm_init(soul_vm_t* vm)
{
	vm->ip = 0;
	vm->sp = 0;

	soul__vm_stack_init(&vm->stack);

	// Clear registers
	memset(vm->registers, 0, VM_REGISTER_COUNT * sizeof(soul_register_t));
}

SOUL_API void soul_vm_free(soul_vm_t* vm)
{
	soul__vm_stack_free(&vm->stack);
	free(vm);

	// @TODO SUPRESS WARNINGS IN THIS FILE
	SOUL_UNUSED(soul__vm_stack_push);
	SOUL_UNUSED(soul__vm_stack_pop);
	SOUL_UNUSED(soul__vm_stack_init);
	SOUL_UNUSED(soul__vm_stack_free);
	SOUL_UNUSED(soul__vm_stack_grow);
	SOUL_UNUSED(soul__vm_stack_peek);
	//
}

