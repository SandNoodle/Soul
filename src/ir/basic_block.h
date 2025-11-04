#pragma once

#include "core/types.h"
#include "ir/instruction.h"

#include <limits>
#include <vector>

namespace soul::ir
{
	class IRBuilder;

	/**
	 * @brief BasicBlock represents a straight-line code sequence with no branches
	 * (with exception to basic block' inputs and outputs).
	 */
	struct BasicBlock
	{
		public:
		using Label        = u32;
		using Instructions = std::vector<std::unique_ptr<Instruction>>;
		using BasicBlocks  = std::vector<BasicBlock*>;

		static constexpr Label k_invalid_label = std::numeric_limits<Label>::max();

		public:
		Label        label{ k_invalid_label };
		BasicBlocks  successors{};
		Instructions instructions{};

		public:
		constexpr BasicBlock() = default;
		constexpr BasicBlock(Label label);
		constexpr BasicBlock(const BasicBlock&)     = delete;
		constexpr BasicBlock(BasicBlock&&) noexcept = default;
		constexpr ~BasicBlock()                     = default;

		constexpr BasicBlock& operator=(const BasicBlock&)                        = delete;
		constexpr BasicBlock& operator=(BasicBlock&&) noexcept                    = default;
		constexpr bool        operator==(const BasicBlock& other) const noexcept  = default;
		constexpr auto        operator<=>(const BasicBlock& other) const noexcept = default;

		friend IRBuilder;
	};
}  // namespace soul::ir
#include "ir/basic_block.inl"
