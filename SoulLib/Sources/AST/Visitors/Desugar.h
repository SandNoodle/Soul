#pragma once

#include "AST/Visitors/copy.h"
#include "AST/ast_fwd.h"

namespace soul::ast::visitors
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
}  // namespace soul::ast::visitors
