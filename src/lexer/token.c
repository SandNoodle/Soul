#include "token.h"

#include "soul_config.h"

#include <stdlib.h>
#include <string.h>

static inline void extend_capacity_if_needed(soul_token_array_t*);

const char* soul_token_type_to_string(soul_token_type_t type)
{
	// IMPORTANT: This function assumes strict order between token types for ranges.
	//            They HAVE to match.
	if(type >= soul_token_semicolon && type <= soul_token_double_pipe)
		return "operator";

	if(type >= soul_token_native && type <= soul_token_false)
		return "keyword";

	// Literals
	if(type == soul_token_number) return "number";
	if(type == soul_token_string) return "string";
	if(type == soul_token_identifier) return "identifier";

	// Special tokens
	if(type == soul_token_error) return "__ERROR__";
	if(type == soul_token_eof) return "__EOF__";

	return "unknown";
}

soul_token_array_t soul_token_array_create(void)
{
	soul_token_array_t array;
	array.tokens   = NULL;
	array.size     = 0;
	array.capacity = 0;
	return array;
}

void soul_token_array_destroy(soul_token_array_t* array)
{
	if (!array) return;
	array->size     = 0;
	array->capacity = 0;
	free(array->tokens);
}

bool soul_token_array_append(soul_token_array_t* array, soul_token_t token)
{
	if (!array) return false;
	extend_capacity_if_needed(array);
	array->tokens[array->size++] = token;
	return true;
}

soul_token_t soul_token_array_at(soul_token_array_t* array, size_t index)
{
	if (!array || index >= array->size)
	{
		const char* error_message = "index out of range";
		return (soul_token_t){.type   = soul_token_error,
		                      .start  = error_message,
		                      .length = strlen(error_message)};
	}
	return array->tokens[index];
}

soul_token_type_t soul_token_array_type_at(soul_token_array_t* array,
                                           size_t index)
{
	if (!array || index >= array->size) return soul_token_error;
	return array->tokens[index].type;
}

soul_token_type_t soul_token_array_type_back(soul_token_array_t* array)
{
	if (!array || array->size == 0) return soul_token_error;
	return array->tokens[array->size - 1].type;
}

bool soul_is_literal_token(soul_token_type_t type)
{
	// clang-format off
	return type == soul_token_identifier
		|| type == soul_token_number
	    || type == soul_token_true
		|| type == soul_token_false
	    || type == soul_token_string;
	// clang-format on
}

bool soul_is_assign_token(soul_token_type_t type)
{
	// clang-format off
	return type == soul_token_equal
		|| type == soul_token_plus_equal
	    || type == soul_token_minus_equal
		|| type == soul_token_star_equal
	    || type == soul_token_slash_equal;
	// clang-format on
}

bool soul_is_sync_token(soul_token_type_t type)
{
	// clang-format off
	return type == soul_token_fn
		|| type == soul_token_let
	    || type == soul_token_if
		|| type == soul_token_for
	    || type == soul_token_while
		|| type == soul_token_return
	    || type == soul_token_struct
		|| type == soul_token_enum
	    || type == soul_token_brace_left; // Scopes
	// clang-format on
}

//
// Private
//

static inline void extend_capacity_if_needed(soul_token_array_t* array)
{
	if (array->size + 1 > array->capacity)
	{
		const size_t new_capacity
		    = array->capacity < SOUL_ARRAY_MIN_CAPACITY
		        ? SOUL_ARRAY_MIN_CAPACITY
		        : array->capacity * SOUL_ARRAY_CAPACITY_GROWTH_RATE;
		array->tokens = (soul_token_t*)realloc(
		    array->tokens, sizeof(soul_token_t) * new_capacity);
		array->capacity = new_capacity;
	}
}
