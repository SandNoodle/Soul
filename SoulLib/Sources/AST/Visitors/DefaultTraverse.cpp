#include "AST/Visitors/DefaultTraverse.h"

namespace Soul::AST::Visitors
{
	void DefaultTraverseVisitor::Accept(ASTNode::Reference node)
	{
		if (!node) {
			return;
		}
		node->Accept(*this);
	}

	void DefaultTraverseVisitor::Visit(const BinaryNode& node)
	{
		Accept(node.lhs.get());
		Accept(node.rhs.get());
	}

	void DefaultTraverseVisitor::Visit(const BlockNode& node)
	{
		for (const auto& statement : node.statements) {
			Accept(statement.get());
		}
	}

	void DefaultTraverseVisitor::Visit(const CastNode& node) { Accept(node.expression.get()); }

	void DefaultTraverseVisitor::Visit([[maybe_unused]] const ErrorNode& node) { /* Can't traverse further. */ }

	void DefaultTraverseVisitor::Visit(const ForLoopNode& node)
	{
		Accept(node.initialization.get());
		Accept(node.condition.get());
		Accept(node.update.get());
		Accept(node.statements.get());
	}

	void DefaultTraverseVisitor::Visit(const ForeachLoopNode& node)
	{
		Accept(node.variable.get());
		Accept(node.in_expression.get());
		Accept(node.statements.get());
	}

	void DefaultTraverseVisitor::Visit(const FunctionCallNode& node)
	{
		for (auto& param : node.parameters) {
			Accept(param.get());
		}
	}

	void DefaultTraverseVisitor::Visit(const FunctionDeclarationNode& node)
	{
		for (auto& param : node.parameters) {
			Accept(param.get());
		}
		Accept(node.statements.get());
	}

	void DefaultTraverseVisitor::Visit(const IfNode& node)
	{
		Accept(node.condition.get());
		Accept(node.then_statements.get());
		Accept(node.else_statements.get());
	}

	void DefaultTraverseVisitor::Visit([[maybe_unused]] const LiteralNode& node) { /* Can't traverse further. */ }

	void DefaultTraverseVisitor::Visit([[maybe_unused]] const LoopControlNode& node) { /* Can't traverse further. */ }

	void DefaultTraverseVisitor::Visit(const ModuleNode& node)
	{
		for (const auto& statement : node.statements) {
			Accept(statement.get());
		}
	}

	void DefaultTraverseVisitor::Visit(const ReturnNode& node) { Accept(node.expression.get()); }

	void DefaultTraverseVisitor::Visit(const StructDeclarationNode& node)
	{
		for (auto& param : node.parameters) {
			Accept(param.get());
		}
	}

	void DefaultTraverseVisitor::Visit(const UnaryNode& node) { Accept(node.expression.get()); }

	void DefaultTraverseVisitor::Visit(const VariableDeclarationNode& node) { Accept(node.expression.get()); }

	void DefaultTraverseVisitor::Visit(const WhileNode& node)
	{
		Accept(node.condition.get());
		Accept(node.statements.get());
	}
}  // namespace Soul::AST::Visitors
