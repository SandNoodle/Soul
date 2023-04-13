#include "token.h"

const char* soul_token_to_string(soul_token_type_t type)
{
	const char* tokens[] = {
		#define TOKEN(x) #x,
		#include "token.inl"
	};

	return tokens[type];
}

void soul_print_token_array(soul_token_vector_t* array, bool pretty_print)
{
	int current_indent = 0;
	for(size_t i = 0; i < array->size - 1; ++i) // Skip EOF.
	{
		const soul_token_t token = array->data[i];
		if(pretty_print)
		{
			if(token.type == TOKEN_BRACE_RIGHT) current_indent--;

			printf("%*c", current_indent * 3, ' ');

			if(token.type == TOKEN_BRACE_LEFT) current_indent++;
		}

		printf("[%s, '%.*s']\n", soul_token_to_string(token.type), (int)token.length, token.start);
	}
}
