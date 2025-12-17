#include "AST/Visitors/copy.h"

namespace soul::ast::visitors
{
	ASTNode::Dependency CopyVisitor::cloned() noexcept { return std::move(_current_clone); }

	void CopyVisitor::visit(const BinaryNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const BlockNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const CastNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const ErrorNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const ForLoopNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const ForeachLoopNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const FunctionCallNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const FunctionDeclarationNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const IfNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const LiteralNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const LoopControlNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const ModuleNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const ReturnNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const StructDeclarationNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const UnaryNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const VariableDeclarationNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::visit(const WhileNode& node)
	{
		_current_clone       = clone(node);
		_current_clone->type = node.type;
	}

	ASTNode::Dependency CopyVisitor::clone(const ASTNode::Reference node)
	{
		if (!node) {
			return nullptr;
		}
		node->accept(*this);
		return std::move(_current_clone);
	}

	ASTNode::Dependency CopyVisitor::clone(const BinaryNode& node)
	{
		auto lhs{ clone(node.lhs.get()) };
		auto rhs{ clone(node.rhs.get()) };
		return BinaryNode::create(std::move(lhs), std::move(rhs), node.op);
	}

	ASTNode::ScopeBlock CopyVisitor::clone(const BlockNode& node) { return BlockNode::create(clone(node.statements)); }

	ASTNode::Dependency CopyVisitor::clone(const CastNode& node)
	{
		return CastNode::create(clone(node.expression.get()), node.type_specifier);
	}

	ASTNode::Dependency CopyVisitor::clone(const ErrorNode& node) { return ErrorNode::create(node.message); }

	ASTNode::Dependency CopyVisitor::clone(const ForLoopNode& node)
	{
		auto initialization{ clone(node.initialization.get()) };
		auto condition{ clone(node.condition.get()) };
		auto update{ clone(node.update.get()) };
		auto statements{ clone(node.statements.get()) };
		return ForLoopNode::create(
			std::move(initialization), std::move(condition), std::move(update), std::move(statements));
	}

	ASTNode::Dependency CopyVisitor::clone(const ForeachLoopNode& node)
	{
		auto variable{ clone(node.variable.get()) };
		auto in_expression{ clone(node.in_expression.get()) };
		auto statements{ clone(node.statements.get()) };
		return ForeachLoopNode::create(std::move(variable), std::move(in_expression), std::move(statements));
	}

	ASTNode::Dependency CopyVisitor::clone(const FunctionCallNode& node)
	{
		return FunctionCallNode::create(node.name, clone(node.parameters));
	}

	ASTNode::Dependency CopyVisitor::clone(const FunctionDeclarationNode& node)
	{
		auto cloned_parameters{ clone(node.parameters) };
		auto statements{ clone(node.statements.get()) };
		return FunctionDeclarationNode::create(
			node.name, node.type_specifier, std::move(cloned_parameters), std::move(statements));
	}

	ASTNode::Dependency CopyVisitor::clone(const IfNode& node)
	{
		auto condition{ clone(node.condition.get()) };
		auto then_statements{ clone(node.then_statements.get()) };
		auto else_statements{ clone(node.else_statements.get()) };
		return IfNode::create(std::move(condition), std::move(then_statements), std::move(else_statements));
	}

	ASTNode::Dependency CopyVisitor::clone(const LiteralNode& node) { return LiteralNode::create(node.value); }

	ASTNode::Dependency CopyVisitor::clone(const LoopControlNode& node)
	{
		return LoopControlNode::create(node.control_type);
	}

	ASTNode::Dependency CopyVisitor::clone(const ModuleNode& node)
	{
		return ModuleNode::create(node.name, clone(node.statements));
	}

	ASTNode::Dependency CopyVisitor::clone(const ReturnNode& node)
	{
		return ReturnNode::create(clone(node.expression.get()));
	}

	ASTNode::Dependency CopyVisitor::clone(const StructDeclarationNode& node)
	{
		return StructDeclarationNode::create(node.name, clone(node.parameters));
	}

	ASTNode::Dependency CopyVisitor::clone(const UnaryNode& node)
	{
		return UnaryNode::create(clone(node.expression.get()), node.op);
	}

	ASTNode::Dependency CopyVisitor::clone(const VariableDeclarationNode& node)
	{
		return VariableDeclarationNode::create(
			node.name, node.type_specifier, clone(node.expression.get()), node.is_mutable);
	}

	ASTNode::Dependency CopyVisitor::clone(const WhileNode& node)
	{
		auto condition{ clone(node.condition.get()) };
		auto statements{ clone(node.statements.get()) };
		return WhileNode::create(std::move(condition), std::move(statements));
	}
}  // namespace soul::ast::visitors
