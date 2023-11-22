#include "token.h"

#include <stdlib.h>

struct soul_token_array_t
{
	soul_token_t* tokens;
	size_t size;
	size_t capacity;
};

soul_token_array_t* soul_token_array_create(void)
{
	soul_token_array_t* array = (soul_token_array_t*)malloc(sizeof(soul_token_array_t));
	array->tokens = NULL;
	array->size = 0;
	array->capacity = 0;
	return array;
}

void soul_token_array_destroy(soul_token_array_t* array)
{
	if(!array) return;
	array->size = 0;
	array->capacity = 0;
	free(array->tokens);
	free(array);
}

bool soul_token_array_append(soul_token_array_t* array, soul_token_t token)
{
	if(!array) return false;
	if(array->size + 1 > array->capacity)
	{
		const size_t new_capacity = array->capacity < 8 ? 8 : array->capacity * 2;
		array->tokens = (soul_token_t*)realloc(array->tokens, sizeof(soul_token_t) * new_capacity);
		array->capacity = new_capacity;
	}

	array->tokens[array->size++] = token;
	return true;
}

soul_token_type_t soul_token_array_type_back(soul_token_array_t* array)
{
	if(!array || array->size == 0) return soul_token_error;
	return array->tokens[array->size - 1].type;
}

bool soul_is_literal(soul_token_type_t type)
{
	return type == soul_token_identifier
		|| type == soul_token_number
		|| type == soul_token_true
		|| type == soul_token_false
		|| type == soul_token_string;
}

bool soul_is_assign(soul_token_type_t type)
{
	return type == soul_token_equal
		|| type == soul_token_plus_equal
		|| type == soul_token_minus_equal
		|| type == soul_token_star_equal
		|| type == soul_token_slash_equal;
}

bool soul_is_sync_token(soul_token_type_t type)
{
	return type == soul_token_fn
		|| type == soul_token_let
		|| type == soul_token_if
		|| type == soul_token_for
		|| type == soul_token_while
		|| type == soul_token_return
		|| type == soul_token_struct
		|| type == soul_token_enum
		|| type == soul_token_brace_left; // Blocks
}
