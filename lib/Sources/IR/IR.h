#pragma once

#include "IR/basic_block.h"
#include "Types/type.h"

#include <string>
#include <string_view>
#include <vector>

namespace Soul::ir
{
	class IRBuilder;

	struct Function
	{
		public:
		std::string name;
		types::Type return_type;
		std::vector<types::Type> parameters;
		std::vector<std::unique_ptr<BasicBlock>> basic_blocks;

		public:
		constexpr Function(std::string_view name, types::Type return_type, std::vector<types::Type> parameters);
	};

	struct Module
	{
		public:
		std::string name;
		std::vector<std::unique_ptr<Function>> functions;

		public:
		constexpr Module(std::string_view name);
	};
}  // namespace Soul::ir
#include "IR/ir.inl"
