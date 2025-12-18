#include "AST/Visitors/Compare.h"

#include <format>
#include <typeindex>

namespace Soul::AST::Visitors
{
	CompareVisitor::operator bool() const noexcept { return _ordering == std::partial_ordering::equivalent; }

	CompareVisitor::operator std::partial_ordering() const noexcept { return _ordering; }

	template <NodeKind Node>
	void compare([[maybe_unused]] const Node& lhs, [[maybe_unused]] const Node& rhs)
	{
		static_assert(false, std::format("not implemented for type: {}", typeid(Node).name()));
	}

	template <>
	void CompareVisitor::Compare<BinaryNode>(const BinaryNode& lhs, const BinaryNode& rhs)
	{
		_ordering = lhs.op <=> rhs.op;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor(lhs.lhs.get(), rhs.lhs.get());
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor(lhs.rhs.get(), rhs.rhs.get());
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
	}

	template <>
	void CompareVisitor::Compare<BlockNode>(const BlockNode& lhs, const BlockNode& rhs)
	{
		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.statements.size() <=> rhs.statements.size();
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
		for (std::size_t index = 0; index < lhs.statements.size(); ++index) {
			_ordering = CompareVisitor(lhs.statements[index].get(), rhs.statements[index].get());
			if (_ordering != std::partial_ordering::equivalent) {
				return;
			}
		}
	}

	template <>
	void CompareVisitor::Compare<CastNode>(const CastNode& lhs, const CastNode& rhs)
	{
		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.type_specifier <=> rhs.type_specifier;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor{ lhs.expression.get(), rhs.expression.get() };
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
	}

	template <>
	void CompareVisitor::Compare<ErrorNode>(const ErrorNode& lhs, const ErrorNode& rhs)
	{
		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.message <=> rhs.message;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
	}

	template <>
	void CompareVisitor::Compare<ForLoopNode>(const ForLoopNode& lhs, const ForLoopNode& rhs)
	{
		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor{ lhs.initialization.get(), rhs.initialization.get() };
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor{ lhs.condition.get(), rhs.condition.get() };
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor{ lhs.update.get(), rhs.update.get() };
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor{ lhs.statements.get(), rhs.statements.get() };
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
	}

	template <>
	void CompareVisitor::Compare<ForeachLoopNode>(const ForeachLoopNode& lhs, const ForeachLoopNode& rhs)
	{
		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor{ lhs.variable.get(), rhs.variable.get() };
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor{ lhs.in_expression.get(), rhs.in_expression.get() };
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor{ lhs.statements.get(), rhs.statements.get() };
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
	}

	template <>
	void CompareVisitor::Compare<FunctionCallNode>(const FunctionCallNode& lhs, const FunctionCallNode& rhs)
	{
		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.name <=> rhs.name;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.parameters.size() <=> rhs.parameters.size();
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
		for (std::size_t index = 0; index < lhs.parameters.size(); ++index) {
			_ordering = CompareVisitor(lhs.parameters[index].get(), rhs.parameters[index].get());
			if (_ordering != std::partial_ordering::equivalent) {
				return;
			}
		}
	}

	template <>
	void CompareVisitor::Compare<FunctionDeclarationNode>(const FunctionDeclarationNode& lhs,
	                                                      const FunctionDeclarationNode& rhs)
	{
		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.name <=> rhs.name;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.type_specifier <=> rhs.type_specifier;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.parameters.size() <=> rhs.parameters.size();
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
		for (std::size_t index = 0; index < lhs.parameters.size(); ++index) {
			_ordering = CompareVisitor(lhs.parameters[index].get(), rhs.parameters[index].get());
			if (_ordering != std::partial_ordering::equivalent) {
				return;
			}
		}

		_ordering = CompareVisitor{ lhs.statements.get(), rhs.statements.get() };
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
	}

	template <>
	void CompareVisitor::Compare<IfNode>(const IfNode& lhs, const IfNode& rhs)
	{
		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor{ lhs.condition.get(), rhs.condition.get() };
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor{ lhs.then_statements.get(), rhs.then_statements.get() };
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor{ lhs.else_statements.get(), rhs.else_statements.get() };
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
	}

	template <>
	void CompareVisitor::Compare<LiteralNode>(const LiteralNode& lhs, const LiteralNode& rhs)
	{
		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.value <=> rhs.value;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
	}

	template <>
	void CompareVisitor::Compare<LoopControlNode>(const LoopControlNode& lhs, const LoopControlNode& rhs)
	{
		_ordering = lhs.control_type <=> rhs.control_type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
	}

	template <>
	void CompareVisitor::Compare<ModuleNode>(const ModuleNode& lhs, const ModuleNode& rhs)
	{
		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.name <=> rhs.name;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.statements.size() <=> rhs.statements.size();
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
		for (std::size_t index = 0; index < lhs.statements.size(); ++index) {
			_ordering = CompareVisitor(lhs.statements[index].get(), rhs.statements[index].get());
			if (_ordering != std::partial_ordering::equivalent) {
				return;
			}
		}
	}

	template <>
	void CompareVisitor::Compare<ReturnNode>(const ReturnNode& lhs, const ReturnNode& rhs)
	{
		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor{ lhs.expression.get(), rhs.expression.get() };
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
	}

	template <>
	void CompareVisitor::Compare<StructDeclarationNode>(const StructDeclarationNode& lhs,
	                                                    const StructDeclarationNode& rhs)
	{
		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.name <=> rhs.name;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.parameters.size() <=> rhs.parameters.size();
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
		for (std::size_t index = 0; index < lhs.parameters.size(); ++index) {
			_ordering = CompareVisitor(lhs.parameters[index].get(), rhs.parameters[index].get());
			if (_ordering != std::partial_ordering::equivalent) {
				return;
			}
		}
	}

	template <>
	void CompareVisitor::Compare<UnaryNode>(const UnaryNode& lhs, const UnaryNode& rhs)
	{
		_ordering = lhs.op <=> rhs.op;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor(lhs.expression.get(), rhs.expression.get());
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
	}

	template <>
	void CompareVisitor::Compare<VariableDeclarationNode>(const VariableDeclarationNode& lhs,
	                                                      const VariableDeclarationNode& rhs)
	{
		_ordering = lhs.is_mutable <=> rhs.is_mutable;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.name <=> rhs.name;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = lhs.type_specifier <=> rhs.type_specifier;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor(lhs.expression.get(), rhs.expression.get());
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
	}

	template <>
	void CompareVisitor::Compare<WhileNode>(const WhileNode& lhs, const WhileNode& rhs)
	{
		_ordering = lhs.type <=> rhs.type;
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor(lhs.condition.get(), rhs.condition.get());
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

		_ordering = CompareVisitor(lhs.statements.get(), rhs.statements.get());
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}
	}

	CompareVisitor::CompareVisitor(ASTNode::Reference lhs, ASTNode::Reference rhs)
	{
		if (lhs == rhs) {
			_ordering = std::partial_ordering::equivalent;
			return;
		}

		if (lhs == nullptr) {
			_ordering = std::partial_ordering::less;
			return;
		}

		if (rhs == nullptr) {
			_ordering = std::partial_ordering::greater;
			return;
		}

		_ordering = std::type_index(typeid(*lhs)) <=> std::type_index(typeid(*rhs));
		if (_ordering != std::partial_ordering::equivalent) {
			return;
		}

#define SOUL_AST_NODE(name)                              \
	if (lhs->Is<name>() && rhs->Is<name>()) {            \
		Compare<name>(lhs->As<name>(), rhs->As<name>()); \
	}
		SOUL_AST_NODES
#undef SOUL_AST_NODE
	}
}  // namespace Soul::AST::Visitors
