#pragma once

#include "AST/ASTFwd.h"
#include "AST/Visitors/Copy.h"

namespace Soul::AST::Visitors
{
	/**
	 * @brief DesugarVisitor traverses the AST while substituting high-level nodes (such as ForNode or ForeachNode)
	 * into a lower-level ones (such as WhileNode).
	 */
	class DesugarVisitor : public CopyVisitor
	{
		public:
		using CopyVisitor::accept;

		protected:
		using CopyVisitor::visit;
		void visit(const BinaryNode&) override;
		void visit(const ForLoopNode&) override;
	};
}  // namespace Soul::AST::Visitors
