#pragma once

#include "soul.h"
#include "parser.h"

typedef struct soul_variable_t soul_variable_t;

SOUL_STACK_DEFINE(variable, soul_variable_t)

struct soul_compiler_t {
	bool had_panic;
	bool had_error;

	soul_variable_stack_t locals;

	soul_chunk_t* current_chunk;

	int32_t current_depth;
};

struct soul_compiler_config_t {
	soul_message_callback_t warn_callback;  // Can be null.
	soul_message_callback_t error_callback; // Can be null.
};
