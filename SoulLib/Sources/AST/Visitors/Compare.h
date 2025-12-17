#pragma once

#include "AST/AST.h"
#include "AST/Visitors/DefaultTraverse.h"

#include <compare>

namespace soul::ast::visitors
{
	/**
	 * @brief CompareVisitor traverses both ASTs and returns the relation between them (by comparing each node).
	 * Useful for determining if a sequence of passes had an impact on the output AST.
	 */
	class CompareVisitor final
	{
		private:
		std::partial_ordering _ordering{ std::partial_ordering::equivalent };

		public:
		CompareVisitor(ASTNode::Reference lhs, ASTNode::Reference rhs);

		operator bool() const noexcept;
		operator std::partial_ordering() const noexcept;

		private:
		template <NodeKind Node>
		void compare(const Node& lhs, const Node& rhs);
	};
}  // namespace soul::ast::visitors
