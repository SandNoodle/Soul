#pragma once

#include "IR/BasicBlock.h"
#include "Types/Type.h"

#include <string>
#include <string_view>
#include <vector>

namespace Soul::IR
{
	class IRBuilder;

	struct Function
	{
		public:
		std::string name;
		Types::Type return_type;
		std::vector<Types::Type> parameters;
		std::vector<std::unique_ptr<BasicBlock>> basic_blocks;

		public:
		constexpr Function(std::string_view name, Types::Type return_type, std::vector<Types::Type> parameters);
	};

	struct Module
	{
		public:
		std::string name;
		std::vector<std::unique_ptr<Function>> functions;

		public:
		constexpr Module(std::string_view name);
	};
}  // namespace Soul::IR
#include "IR/IR.inl"
