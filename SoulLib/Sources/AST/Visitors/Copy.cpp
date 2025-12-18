#include "AST/Visitors/Copy.h"

namespace Soul::AST::Visitors
{
	ASTNode::Dependency CopyVisitor::Cloned() noexcept { return std::move(_current_clone); }

	void CopyVisitor::Visit(const BinaryNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const BlockNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const CastNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const ErrorNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const ForLoopNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const ForeachLoopNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const FunctionCallNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const FunctionDeclarationNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const IfNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const LiteralNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const LoopControlNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const ModuleNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const ReturnNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const StructDeclarationNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const UnaryNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const VariableDeclarationNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}
	void CopyVisitor::Visit(const WhileNode& node)
	{
		_current_clone       = Clone(node);
		_current_clone->type = node.type;
	}

	ASTNode::Dependency CopyVisitor::Clone(const ASTNode::Reference node)
	{
		if (!node) {
			return nullptr;
		}
		node->Accept(*this);
		return std::move(_current_clone);
	}

	ASTNode::Dependency CopyVisitor::Clone(const BinaryNode& node)
	{
		auto lhs{ Clone(node.lhs.get()) };
		auto rhs{ Clone(node.rhs.get()) };
		return BinaryNode::Create(std::move(lhs), std::move(rhs), node.op);
	}

	ASTNode::ScopeBlock CopyVisitor::Clone(const BlockNode& node) { return BlockNode::Create(Clone(node.statements)); }

	ASTNode::Dependency CopyVisitor::Clone(const CastNode& node)
	{
		return CastNode::Create(Clone(node.expression.get()), node.type_specifier);
	}

	ASTNode::Dependency CopyVisitor::Clone(const ErrorNode& node) { return ErrorNode::Create(node.message); }

	ASTNode::Dependency CopyVisitor::Clone(const ForLoopNode& node)
	{
		auto initialization{ Clone(node.initialization.get()) };
		auto condition{ Clone(node.condition.get()) };
		auto update{ Clone(node.update.get()) };
		auto statements{ Clone(node.statements.get()) };
		return ForLoopNode::Create(
			std::move(initialization), std::move(condition), std::move(update), std::move(statements));
	}

	ASTNode::Dependency CopyVisitor::Clone(const ForeachLoopNode& node)
	{
		auto variable{ Clone(node.variable.get()) };
		auto in_expression{ Clone(node.in_expression.get()) };
		auto statements{ Clone(node.statements.get()) };
		return ForeachLoopNode::Create(std::move(variable), std::move(in_expression), std::move(statements));
	}

	ASTNode::Dependency CopyVisitor::Clone(const FunctionCallNode& node)
	{
		return FunctionCallNode::Create(node.name, Clone(node.parameters));
	}

	ASTNode::Dependency CopyVisitor::Clone(const FunctionDeclarationNode& node)
	{
		auto cloned_parameters{ Clone(node.parameters) };
		auto statements{ Clone(node.statements.get()) };
		return FunctionDeclarationNode::Create(
			node.name, node.type_specifier, std::move(cloned_parameters), std::move(statements));
	}

	ASTNode::Dependency CopyVisitor::Clone(const IfNode& node)
	{
		auto condition{ Clone(node.condition.get()) };
		auto then_statements{ Clone(node.then_statements.get()) };
		auto else_statements{ Clone(node.else_statements.get()) };
		return IfNode::Create(std::move(condition), std::move(then_statements), std::move(else_statements));
	}

	ASTNode::Dependency CopyVisitor::Clone(const LiteralNode& node) { return LiteralNode::Create(node.value); }

	ASTNode::Dependency CopyVisitor::Clone(const LoopControlNode& node)
	{
		return LoopControlNode::Create(node.control_type);
	}

	ASTNode::Dependency CopyVisitor::Clone(const ModuleNode& node)
	{
		return ModuleNode::Create(node.name, Clone(node.statements));
	}

	ASTNode::Dependency CopyVisitor::Clone(const ReturnNode& node)
	{
		return ReturnNode::Create(Clone(node.expression.get()));
	}

	ASTNode::Dependency CopyVisitor::Clone(const StructDeclarationNode& node)
	{
		return StructDeclarationNode::Create(node.name, Clone(node.parameters));
	}

	ASTNode::Dependency CopyVisitor::Clone(const UnaryNode& node)
	{
		return UnaryNode::Create(Clone(node.expression.get()), node.op);
	}

	ASTNode::Dependency CopyVisitor::Clone(const VariableDeclarationNode& node)
	{
		return VariableDeclarationNode::Create(
			node.name, node.type_specifier, Clone(node.expression.get()), node.is_mutable);
	}

	ASTNode::Dependency CopyVisitor::Clone(const WhileNode& node)
	{
		auto condition{ Clone(node.condition.get()) };
		auto statements{ Clone(node.statements.get()) };
		return WhileNode::Create(std::move(condition), std::move(statements));
	}
}  // namespace Soul::AST::Visitors
