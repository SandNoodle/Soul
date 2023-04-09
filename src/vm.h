#pragma once

#include "soul.h"

#include "opcode.h"
#include "compiler.h"

typedef struct {
	soul_value_t* data;
	uint32_t size;
	uint32_t capacity;

} soul_stack_t;

#define VM_REGISTER_COUNT 8
#define VM_STACK_MIN_SIZE 2048

struct soul_vm_t {
	soul_register_t registers[VM_REGISTER_COUNT];
	soul_stack_t stack;

	uint32_t ip; // Instruction pointer
	uint32_t sp; // Stack pointer
};
