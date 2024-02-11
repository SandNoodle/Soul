#include "value.h"

#include "soul_config.h"

#include <stdlib.h>
#include <string.h>

#define RETURN_VALUE_IF_ERROR(cond) if ((cond)) { return (soul_value_t){0}; }

soul_value_stack_t soul_value_stack_create(soul_allocator_t* allocator)
{
	soul_value_stack_t stack;
	stack.values    = NULL;
	stack.size      = 0;
	stack.capacity  = 0;
	stack.allocator = allocator;
	return stack;
}

void soul_value_stack_destroy(soul_value_stack_t* stack)
{
	if(!stack) return;
	stack->allocator->free(stack->values, stack->capacity * sizeof(soul_value_t), stack->allocator->user_data);
	stack->values    = NULL;
	stack->size      = 0;
	stack->capacity  = 0;
	stack->allocator = NULL;
}

void soul_value_stack_push(soul_value_stack_t* stack, soul_value_t value)
{
	if(!stack) return;
	if(stack->size + 1 > stack->capacity)
	{
		soul_allocator_t* alloc = stack->allocator;
		const size_t new_capacity
			= stack->capacity < SOUL_STACK_MIN_CAPACITY
			? SOUL_STACK_MIN_CAPACITY
			: stack->capacity * SOUL_STACK_CAPACITY_GROWTH_RATE;
		stack->values = (soul_value_t*)alloc->realloc(
		    stack->values,
		    sizeof(soul_value_t) * new_capacity,
		    alloc->user_data);
		stack->capacity = new_capacity;
	}
	stack->values[stack->size++] = value;
}

soul_value_t soul_value_stack_pop(soul_value_stack_t* stack)
{
	RETURN_VALUE_IF_ERROR(!stack);
	RETURN_VALUE_IF_ERROR(stack->size < 1);
	return stack->values[stack->size--];
}

void soul_value_stack_pop_n(soul_value_stack_t* stack, size_t count)
{
	if(!stack) return;
	// TODO Handle possiblity of size_t underflow due to subtraction.
	stack->size -= count;
}

soul_value_t soul_value_stack_peek(soul_value_stack_t* stack)
{
	RETURN_VALUE_IF_ERROR(!stack);
	return stack->values[stack->size];
}

soul_value_t soul_value_stack_peek_n(soul_value_stack_t* stack, size_t relative_index)
{
	RETURN_VALUE_IF_ERROR(!stack);
	return stack->size >= relative_index ? stack->values[stack->size - relative_index] : stack->values[0];
}

void soul_value_stack_reset(soul_value_stack_t* stack)
{
	if(!stack) return;
	stack->size = 0;
	memset(stack->values, 0, sizeof(soul_value_t) * stack->capacity);
}

soul_value_array_t soul_value_array_create(soul_allocator_t* allocator)
{
	soul_value_array_t array;
	array.values    = NULL;
	array.size      = 0;
	array.capacity  = 0;
	array.allocator = allocator;
	return array;
}

void soul_value_array_destroy(soul_value_array_t* array)
{
	if(!array) return;
	array->allocator->free(array->values, array->capacity * sizeof(soul_value_t), array->allocator->user_data);
	array->values    = NULL;
	array->size      = 0;
	array->capacity  = 0;
	array->allocator = NULL;
}

bool soul_value_array_append(soul_value_array_t* array, soul_value_t value)
{
	if(!array) return false;
	if(array->size + 1 > array->capacity)
	{
		soul_allocator_t* alloc = array->allocator;
		const size_t new_capacity
			= array->capacity < SOUL_STACK_MIN_CAPACITY
			? SOUL_STACK_MIN_CAPACITY
			: array->capacity * SOUL_STACK_CAPACITY_GROWTH_RATE;
		array->values = (soul_value_t*)alloc->realloc(array->values, sizeof(soul_value_t) * new_capacity, alloc->user_data);
		array->capacity = new_capacity;
	}
	array->values[array->size++] = value;
	return true;
}

soul_value_t soul_value_array_at(soul_value_array_t* array, size_t index)
{
	RETURN_VALUE_IF_ERROR(!array);
	RETURN_VALUE_IF_ERROR(index >= array->size);
	return array->values[index];
}

#undef RETURN_VALUE_IF_ERROR
