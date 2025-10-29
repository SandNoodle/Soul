#pragma once

#include <concepts>

namespace soul::ir
{
	struct BasicBlock;
	struct Function;
	struct Instruction;
	struct Module;

	/**
	 * @brief InstructionKind is a concept, which holds true for any class that inherits from an Instruction class.
	 */
	template <typename T>
	concept InstructionKind = std::derived_from<T, Instruction>;

/**
 * @brief Arithmetic instructions: performs an arithmetic operation on two expressions.
 */
#define SOUL_ARITHMETIC_INSTRUCTIONS \
	SOUL_INSTRUCTION(Add)            \
	SOUL_INSTRUCTION(Sub)            \
	SOUL_INSTRUCTION(Mul)            \
	SOUL_INSTRUCTION(Div)            \
	SOUL_INSTRUCTION(Mod)

/**
 * @brief Comparison instructions: compare two expressions; returns boolean result.
 */
#define SOUL_COMPARISON_INSTRUCTIONS \
	SOUL_INSTRUCTION(Equal)          \
	SOUL_INSTRUCTION(NotEqual)       \
	SOUL_INSTRUCTION(Greater)        \
	SOUL_INSTRUCTION(GreaterEqual)   \
	SOUL_INSTRUCTION(Less)           \
	SOUL_INSTRUCTION(LessEqual)

/**
 * @brief Logical instructions: perform boolean arithmetic on two expressions; returns boolean result.
 */
#define SOUL_LOGICAL_INSTRUCTIONS \
	SOUL_INSTRUCTION(And)         \
	SOUL_INSTRUCTION(Or)

/**
 * @brief List of all instructions; useful for x-macros.
 */
#define SOUL_ALL_INSTRUCTIONS     \
	SOUL_INSTRUCTION(Unreachable) \
	SOUL_INSTRUCTION(Noop)        \
	SOUL_INSTRUCTION(Call)        \
	SOUL_INSTRUCTION(Cast)        \
	SOUL_INSTRUCTION(Const)       \
	SOUL_INSTRUCTION(Jump)        \
	SOUL_INSTRUCTION(JumpIf)      \
	SOUL_INSTRUCTION(Return)      \
	SOUL_INSTRUCTION(StackSlot)   \
	SOUL_INSTRUCTION(StackStore)  \
	SOUL_INSTRUCTION(StackLoad)   \
	SOUL_INSTRUCTION(GetArgument) \
	SOUL_INSTRUCTION(Phi)         \
	SOUL_INSTRUCTION(Upsilon)     \
	SOUL_INSTRUCTION(Not)         \
	SOUL_ARITHMETIC_INSTRUCTIONS  \
	SOUL_COMPARISON_INSTRUCTIONS  \
	SOUL_LOGICAL_INSTRUCTIONS

#define SOUL_INSTRUCTION(name) struct name;
	SOUL_ALL_INSTRUCTIONS
#undef SOUL_INSTRUCTION
}  // namespace soul::ir
