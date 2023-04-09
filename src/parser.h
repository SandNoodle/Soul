#pragma once

#include "soul.h"

#include "token.h"
#include "ast.h"

typedef struct
{
	soul_token_array_t* tokens;
	size_t tokens_index;

	soul_token_t current_token;

	bool had_panic;
	bool had_error;

	soul_message_callback_t error_callback;
	soul_message_callback_t warn_callback;
} soul_parser_t;
