#pragma once

typedef enum {
	#define OPCODE(x) x,
	#include "opcode.inl"
} opcode_t;

const char* opcode_to_string(opcode_t type);
