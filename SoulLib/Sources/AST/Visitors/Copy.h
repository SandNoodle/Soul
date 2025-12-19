#pragma once

#include "AST/AST.h"
#include "AST/ASTFwd.h"
#include "AST/Visitors/DefaultTraverse.h"

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
		ASTNode::Dependency Cloned() noexcept;

		using DefaultTraverseVisitor::Accept;

		[[nodiscard]] constexpr bool Affects() const noexcept override { return true; }

		protected:
		using DefaultTraverseVisitor::Visit;
		void Visit(const BinaryNode&) override;
		void Visit(const BlockNode&) override;
		void Visit(const CastNode&) override;
		void Visit(const ErrorNode&) override;
		void Visit(const ForLoopNode&) override;
		void Visit(const ForeachLoopNode&) override;
		void Visit(const FunctionCallNode&) override;
		void Visit(const FunctionDeclarationNode&) override;
		void Visit(const IfNode&) override;
		void Visit(const LiteralNode&) override;
		void Visit(const LoopControlNode&) override;
		void Visit(const ModuleNode&) override;
		void Visit(const ReturnNode&) override;
		void Visit(const StructDeclarationNode&) override;
		void Visit(const UnaryNode&) override;
		void Visit(const VariableDeclarationNode&) override;
		void Visit(const WhileNode&) override;

		ASTNode::Dependency Clone(const ASTNode::Reference node);
		ASTNode::Dependency Clone(const BinaryNode&);
		ASTNode::ScopeBlock Clone(const BlockNode&);
		ASTNode::Dependency Clone(const CastNode&);
		ASTNode::Dependency Clone(const ErrorNode&);
		ASTNode::Dependency Clone(const ForLoopNode&);
		ASTNode::Dependency Clone(const ForeachLoopNode&);
		ASTNode::Dependency Clone(const FunctionCallNode&);
		ASTNode::Dependency Clone(const FunctionDeclarationNode&);
		ASTNode::Dependency Clone(const IfNode&);
		ASTNode::Dependency Clone(const LiteralNode&);
		ASTNode::Dependency Clone(const LoopControlNode&);
		ASTNode::Dependency Clone(const ModuleNode&);
		ASTNode::Dependency Clone(const ReturnNode&);
		ASTNode::Dependency Clone(const StructDeclarationNode&);
		ASTNode::Dependency Clone(const UnaryNode&);
		ASTNode::Dependency Clone(const VariableDeclarationNode&);
		ASTNode::Dependency Clone(const WhileNode&);
		ASTNode::Dependencies Clone(const std::ranges::forward_range auto& elements)
		{
			ASTNode::Dependencies cloned{};
			cloned.reserve(elements.size());
			for (const auto& element : elements) {
				cloned.emplace_back(Clone(element.get()));
			}
			return cloned;
		}
	};
}  // namespace Soul::AST::Visitors
