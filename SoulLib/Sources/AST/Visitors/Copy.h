#pragma once

#include "AST/Visitors/DefaultTraverse.h"
#include "AST/AST.h"
#include "AST/ASTFwd.h"

#include <cassert>

namespace Soul::AST::Visitors
{
	/**
	 * @brief CopyVisitor traverses the AST and performs a deep copy on each node, which is then put into a separate
	 * AST. Both trees are equal to each other, but exist as a separate objects in memory. It's most useful when a
	 * visitor can/must modify the input tree.
	 */
	class CopyVisitor : public DefaultTraverseVisitor
	{
		protected:
		ASTNode::Dependency _current_clone{};

		public:
		ASTNode::Dependency cloned() noexcept;

		using DefaultTraverseVisitor::accept;

		[[nodiscard]] constexpr bool affects() const noexcept override { return true; }

		protected:
		using DefaultTraverseVisitor::visit;
		void visit(const BinaryNode&) override;
		void visit(const BlockNode&) override;
		void visit(const CastNode&) override;
		void visit(const ErrorNode&) override;
		void visit(const ForLoopNode&) override;
		void visit(const ForeachLoopNode&) override;
		void visit(const FunctionCallNode&) override;
		void visit(const FunctionDeclarationNode&) override;
		void visit(const IfNode&) override;
		void visit(const LiteralNode&) override;
		void visit(const LoopControlNode&) override;
		void visit(const ModuleNode&) override;
		void visit(const ReturnNode&) override;
		void visit(const StructDeclarationNode&) override;
		void visit(const UnaryNode&) override;
		void visit(const VariableDeclarationNode&) override;
		void visit(const WhileNode&) override;

		ASTNode::Dependency clone(const ASTNode::Reference node);
		ASTNode::Dependency clone(const BinaryNode&);
		ASTNode::ScopeBlock clone(const BlockNode&);
		ASTNode::Dependency clone(const CastNode&);
		ASTNode::Dependency clone(const ErrorNode&);
		ASTNode::Dependency clone(const ForLoopNode&);
		ASTNode::Dependency clone(const ForeachLoopNode&);
		ASTNode::Dependency clone(const FunctionCallNode&);
		ASTNode::Dependency clone(const FunctionDeclarationNode&);
		ASTNode::Dependency clone(const IfNode&);
		ASTNode::Dependency clone(const LiteralNode&);
		ASTNode::Dependency clone(const LoopControlNode&);
		ASTNode::Dependency clone(const ModuleNode&);
		ASTNode::Dependency clone(const ReturnNode&);
		ASTNode::Dependency clone(const StructDeclarationNode&);
		ASTNode::Dependency clone(const UnaryNode&);
		ASTNode::Dependency clone(const VariableDeclarationNode&);
		ASTNode::Dependency clone(const WhileNode&);
		ASTNode::Dependencies clone(const std::ranges::forward_range auto& elements)
		{
			ASTNode::Dependencies cloned{};
			cloned.reserve(elements.size());
			for (const auto& element : elements) {
				cloned.emplace_back(clone(element.get()));
			}
			return cloned;
		}
	};
}  // namespace Soul::ast::visitors
