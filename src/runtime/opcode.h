#ifndef RUNTIME_OPCODE_H
#define RUNTIME_OPCODE_H

#include <stdint.h>

namespace soul
{
	enum class opcode : uint8_t
	{
		op_noop, // Does nothing.

		// Stack operations
		op_pop,
		op_get_local,
		op_set_local,
		op_get_const,
		op_push_true,  // Pushes 'true' bool literal
		op_push_false, // Pushes 'false' bool literal

		// Control flow
		op_jump,       // Unconditional jump
		op_jump_false, // Conditional jump when false
		op_jump_true,  // Conditional jump when true

		// Arithmetic
		op_addi, op_subi, op_muli, op_divi, // Integer
		op_addf, op_subf, op_mulf, op_divf, // Real

		//
		op_return,
		op_halt,
	};
} // namespace soul

#endif // RUNTIME_OPCODE_H
