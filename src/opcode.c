#include "opcode.h"

const char* opcode_to_string(opcode_t type)
{
	const char* opcodes[] = {
		#define OPCODE(x) #x,
		#include "opcode.inl"
	};

	return opcodes[type];
}
