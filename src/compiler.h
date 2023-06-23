#pragma once

#include "soul.h"
#include "parser.h"

typedef struct {
	bool had_panic;
	bool had_error;

	soul_chunk_t* current_chunk;

	int32_t current_depth;
} soul_compiler_t;

struct soul_compiler_config_t {
	soul_message_callback_t warn_callback;  // Can be null.
	soul_message_callback_t error_callback; // Can be null.
};
