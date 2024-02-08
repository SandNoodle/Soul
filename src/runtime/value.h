#ifndef SOUL_RUNTIME_VALUE_H
#define SOUL_RUNTIME_VALUE_H

#include <stdint.h>
#include <stdbool.h>

/** */
typedef union soul_value_t soul_value_t;
union soul_value_t
{
	int64_t int_value;
	double float_value;
	char* string_value;
};

typedef struct soul_value_stack_t soul_value_stack_t;
struct soul_value_stack_t
{
	soul_value_t* values;
	size_t size;
	size_t capacity;
};

/** Creates a stack that can holds values. */
soul_value_stack_t soul_value_stack_create(void);

/** Destroys given soul_value_stack. */
void soul_value_stack_destroy(soul_value_stack_t* stack);

/** Pushes given value on top of the stack. */
void soul_value_stack_push(soul_value_stack_t* stack, soul_value_t value);

/** Pops value from the stack and returns it. */
soul_value_t soul_value_stack_pop(soul_value_stack_t* stack);

/** Pops N values from the stack - returns nothing. */
void soul_value_stack_pop_n(soul_value_stack_t* stack, size_t count);

/** Peeks and returns the value from on top of the stack. */
soul_value_t soul_value_stack_peek(soul_value_stack_t* stack);

/** Peeks value at the position N counting back from the top of the stack. */
soul_value_t soul_value_stack_peek_n(soul_value_stack_t* stack, size_t relative_index);

#endif //SOUL_RUNTIME_VALUE_H
