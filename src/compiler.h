#pragma once

#include "soul.h"
#include "parser.h"

SOUL_VECTOR_DEFINE(chunk_constants, soul_value_t)
SOUL_VECTOR_DEFINE(chunk_data, uint8_t)

struct soul_chunk_t {
	soul_chunk_data_vector_t code;
	soul_chunk_constants_vector_t constants;

	soul_valid_t valid;
};

typedef struct {
	bool had_panic;
	bool had_error;
} soul_compiler_t;
