#ifndef SOUL_RUNTIME_OPCODE_H
#define SOUL_RUNTIME_OPCODE_H

#include <stdint.h>

typedef enum soul_opcode_t : uint8_t
{
	soul_op_noop, // Does nothing.

	// Stack operations
	soul_op_pop,
	soul_op_get_local,
	soul_op_set_local,
	soul_op_get_const,
	soul_op_push_true,  // Pushes 'true' bool literal
	soul_op_push_false, // Pushes 'false' bool literal

	// Control flow
	soul_op_jump,       // Unconditional jump
	soul_op_jump_false, // Conditional jump when false
	soul_op_jump_true,  // Conditional jump when true

	// Arithmetic
	soul_op_addi, soul_op_subi, soul_op_muli, soul_op_divi, // Integer
	soul_op_addf, soul_op_subf, soul_op_mulf, soul_op_divf, // Real

	// Special opcodes.
	soul_op_halt,
} soul_opcode_t;

#endif // SOUL_RUNTIME_OPCODE_H
