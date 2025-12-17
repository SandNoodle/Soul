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
		std::vector<Type> input_types;
		Type return_type;

		/** @brief Returns list of all builtin operations between types. */
		static std::span<const OperatorOverload> All() noexcept;
	};

}  // namespace Soul::Types
