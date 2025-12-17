#pragma once

#include "IR/InstructionFwd.h"

#include <format>
#include <sstream>
#include <string>
#include <string_view>

namespace Soul::IR::Visitors
{
	/**
	 * @brief PrintVisitor traverses the ir::Module, visiting each function (and its BasicBlocks), which converting
	 * each Instruction into it's textual representation. Its useful for debugging.
	 */
	class PrintVisitor
	{
		private:
		std::stringstream _ss;

		public:
		/** @brief Returns textual representation of an IR. */
		std::string string() const;

		void accept(const Module& module);

		private:
#define SOUL_INSTRUCTION(name) void visit(const name&);
		SOUL_ALL_INSTRUCTIONS
#undef SOUL_INSTRUCTION
	};
}  // namespace Soul::IR::Visitors
