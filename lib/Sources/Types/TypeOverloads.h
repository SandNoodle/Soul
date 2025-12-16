#pragma once

#include "AST/ast.h"

#include <span>
#include <vector>

namespace Soul::types
{
	/**
	 * @brief Represents types between which given operation is valid, and what type it results in.
	 */
	struct OperatorOverload
	{
		ast::ASTNode::Operator op;
		std::vector<types::Type> input_types;
		types::Type return_type;

		/** @brief Returns list of all builtin operations between types. */
		static std::span<const OperatorOverload> all() noexcept;
	};

}  // namespace Soul::types
