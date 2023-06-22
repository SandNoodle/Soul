#pragma once

#include "soul.h"
#include "parser.h"

typedef struct {
	bool had_panic;
	bool had_error;

	soul_chunk_t* current_chunk;

	int32_t current_depth;
} soul_compiler_t;
