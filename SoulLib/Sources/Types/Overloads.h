#pragma once

#include "AST/AST.h"

#include <span>
#include <vector>

namespace Soul::Types
{
	/**
	 * @brief Represents types between which given operation is valid, and what type it results in.
	 */
	struct OperatorOverload
	{
		AST::ASTNode::Operator op;
		std::vector<Types::Type> input_types;
		Types::Type return_type;

		/** @brief Returns list of all builtin operations between types. */
		static std::span<const OperatorOverload> all() noexcept;
	};

}  // namespace Soul::Types
