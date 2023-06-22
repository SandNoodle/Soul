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
