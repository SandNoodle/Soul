#pragma once

#include "AST/AST.h"
#include "AST/ASTFwd.h"
#include "AST/Visitors/Visitor.h"

#include <functional>

namespace Soul::AST::Visitors
{
	/**
	 * @brief DefaultTraverseVisitor traverses every node of the Abstract Syntax Tree.
	 * It's useful for getting into a specific node.
	 */
	class DefaultTraverseVisitor : public IVisitor
	{
		public:
		virtual ~DefaultTraverseVisitor() = default;

		virtual void Accept(ASTNode::Reference node);

		protected:
		virtual void Visit(const BinaryNode&) override;
		virtual void Visit(const BlockNode&) override;
		virtual void Visit(const CastNode&) override;
		virtual void Visit(const ErrorNode&) override;
		virtual void Visit(const ForLoopNode&) override;
		virtual void Visit(const ForeachLoopNode&) override;
		virtual void Visit(const FunctionCallNode&) override;
		virtual void Visit(const FunctionDeclarationNode&) override;
		virtual void Visit(const IfNode&) override;
		virtual void Visit(const LiteralNode&) override;
		virtual void Visit(const LoopControlNode&) override;
		virtual void Visit(const ModuleNode&) override;
		virtual void Visit(const ReturnNode&) override;
		virtual void Visit(const StructDeclarationNode&) override;
		virtual void Visit(const UnaryNode&) override;
		virtual void Visit(const VariableDeclarationNode&) override;
		virtual void Visit(const WhileNode&) override;
	};
}  // namespace Soul::AST::Visitors
