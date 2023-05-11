#pragma once

#include "soul.h"
#include "parser.h"

typedef struct {
	uint32_t count;
	uint32_t capacity;
	soul_value_t* values;
} soul_chunk_constants_t;

struct soul_chunk_t {
	uint32_t size;
	uint32_t capacity;
	uint8_t* data;

	soul_chunk_constants_t constants;

	soul_valid_t valid;
};

typedef struct {
	bool had_panic;
	bool had_error;
} soul_compiler_t;
