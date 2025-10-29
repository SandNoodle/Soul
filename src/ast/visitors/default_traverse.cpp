#include "ast/visitors/default_traverse.h"

namespace soul::ast::visitors
{
	void DefaultTraverseVisitor::accept(ASTNode::Reference node)
	{
		if (!node) {
			return;
		}
		node->accept(*this);
	}

	void DefaultTraverseVisitor::visit(const BinaryNode& node)
	{
		accept(node.lhs.get());
		accept(node.rhs.get());
	}

	void DefaultTraverseVisitor::visit(const BlockNode& node)
	{
		for (const auto& statement : node.statements) {
			accept(statement.get());
		}
	}

	void DefaultTraverseVisitor::visit(const CastNode& node) { accept(node.expression.get()); }

	void DefaultTraverseVisitor::visit([[maybe_unused]] const ErrorNode& node) { /* Can't traverse further. */ }

	void DefaultTraverseVisitor::visit(const ForLoopNode& node)
	{
		accept(node.initialization.get());
		accept(node.condition.get());
		accept(node.update.get());
		accept(node.statements.get());
	}

	void DefaultTraverseVisitor::visit(const ForeachLoopNode& node)
	{
		accept(node.variable.get());
		accept(node.in_expression.get());
		accept(node.statements.get());
	}

	void DefaultTraverseVisitor::visit(const FunctionCallNode& node)
	{
		for (auto& param : node.parameters) {
			accept(param.get());
		}
	}

	void DefaultTraverseVisitor::visit(const FunctionDeclarationNode& node)
	{
		for (auto& param : node.parameters) {
			accept(param.get());
		}
		accept(node.statements.get());
	}

	void DefaultTraverseVisitor::visit(const IfNode& node)
	{
		accept(node.condition.get());
		accept(node.then_statements.get());
		accept(node.else_statements.get());
	}

	void DefaultTraverseVisitor::visit([[maybe_unused]] const LiteralNode& node) { /* Can't traverse further. */ }

	void DefaultTraverseVisitor::visit([[maybe_unused]] const LoopControlNode& node) { /* Can't traverse further. */ }

	void DefaultTraverseVisitor::visit(const ModuleNode& node)
	{
		for (const auto& statement : node.statements) {
			accept(statement.get());
		}
	}

	void DefaultTraverseVisitor::visit(const ReturnNode& node) { accept(node.expression.get()); }

	void DefaultTraverseVisitor::visit(const StructDeclarationNode& node)
	{
		for (auto& param : node.parameters) {
			accept(param.get());
		}
	}

	void DefaultTraverseVisitor::visit(const UnaryNode& node) { accept(node.expression.get()); }

	void DefaultTraverseVisitor::visit(const VariableDeclarationNode& node) { accept(node.expression.get()); }

	void DefaultTraverseVisitor::visit(const WhileNode& node)
	{
		accept(node.condition.get());
		accept(node.statements.get());
	}
}  // namespace soul::ast::visitors
