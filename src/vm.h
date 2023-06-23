#pragma once

#include "soul.h"

#include "opcode.h"
#include "compiler.h"

SOUL_STACK_DEFINE(value, soul_value_t)

struct soul_vm_t {
	soul_value_stack_t stack;

	uint32_t ip; // Instruction pointer

	bool had_panic;
	bool had_error;
};

struct soul_chunk_t {
	soul_chunk_data_vector_t code;
	soul_chunk_constants_vector_t constants;
	bool valid;
};

// This struct contains configuration options for a runtime enviroment.
struct soul_config_t {
	size_t vm_stack_size_initial;
	size_t vm_stack_size_max;               // If 0, then maximum size will be unboud.

	soul_message_callback_t warn_callback;  // Can be null.
	soul_message_callback_t error_callback; // Can be null.
};
