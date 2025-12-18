#include "AST/Visitors/SemanticAnalyzer.h"

#include "AST/AST.h"

namespace Soul::AST::Visitors
{
	void SemanticAnalyzerVisitor::Visit(const BinaryNode& node)
	{
		CopyVisitor::Visit(node);

		const auto& binary_node = _current_clone->As<BinaryNode>();
		if (binary_node.op == ASTNode::Operator::ASSIGN && binary_node.lhs->Is<LiteralNode>()) {
			const auto& literal = binary_node.lhs->As<LiteralNode>();
			if (literal.value.Is<Identifier>()) {
				const auto& identifier            = std::string(literal.value.As<Identifier>());
				VariableDeclarationNode* variable = GetVariable(identifier);
				if (!variable) {
					_current_clone = ErrorNode::Create(
						std::format("[INTERNAL] identifier '{}' should've been checked at this point.", identifier));
					return;
				}
				if (!variable->is_mutable) {
					_current_clone = ErrorNode::Create(
						std::format("cannot assign to variable '{}', because it is not mutable.", identifier));
					return;
				}
			}
		}
	}

	void SemanticAnalyzerVisitor::Visit(const BlockNode& node)
	{
		// ENTER SCOPE: Mark a restorepoint for variables declared in the current scope.
		_current_depth++;
		const std::size_t variables_until_this_point = _variables_in_scope.size();

		CopyVisitor::Visit(node);
		// EXIT SCOPE: Remove variables defined in that scope.
		const std::size_t variables_declared_in_scope = _variables_in_scope.size() - variables_until_this_point;
		_variables_in_scope.erase(_variables_in_scope.end() - static_cast<std::ptrdiff_t>(variables_declared_in_scope),
		                          _variables_in_scope.end());
		_current_depth--;
	}

	void SemanticAnalyzerVisitor::Visit(const ForLoopNode& node)
	{
		const bool was_in_loop = _is_in_loop;
		_is_in_loop            = true;
		CopyVisitor::Visit(node);
		_is_in_loop = was_in_loop;
	}

	void SemanticAnalyzerVisitor::Visit(const ForeachLoopNode& node)
	{
		const bool was_in_loop = _is_in_loop;
		_is_in_loop            = true;
		CopyVisitor::Visit(node);
		_is_in_loop = was_in_loop;
	}

	void SemanticAnalyzerVisitor::Visit(const FunctionDeclarationNode& node)
	{
		_variables_in_scope.clear();
		CopyVisitor::Visit(node);
	}

	void SemanticAnalyzerVisitor::Visit(const LiteralNode& node)
	{
		if (node.value.Is<Identifier>()) {
			const auto& identifier            = node.value.As<Identifier>();
			VariableDeclarationNode* variable = GetVariable(identifier);
			if (!variable) {
				_current_clone
					= ErrorNode::Create(std::format("use of an undeclared identifier '{}'", std::string(identifier)));

				return;
			}
		}
		CopyVisitor::Visit(node);
	}

	void SemanticAnalyzerVisitor::Visit(const LoopControlNode& node)
	{
		if (!_is_in_loop) {
			_current_clone = ErrorNode::Create(
				std::format("keyword '{}' must be used in a loop context.",
			                node.control_type == LoopControlNode::Type::BREAK ? "break" : "continue"));
			return;
		}
		CopyVisitor::Visit(node);
	}

	void SemanticAnalyzerVisitor::Visit(const VariableDeclarationNode& node)
	{
		if (_current_depth == 0) {
			_current_clone
				= ErrorNode::Create(std::format("variable '{}' cannot be declared in the global scope.", node.name));
			return;
		}
		CopyVisitor::Visit(node);
		_variables_in_scope.emplace_back(&_current_clone->As<VariableDeclarationNode>());
	}

	void SemanticAnalyzerVisitor::Visit(const WhileNode& node)
	{
		const bool was_in_loop = _is_in_loop;
		_is_in_loop            = true;
		CopyVisitor::Visit(node);
		_is_in_loop = was_in_loop;
	}

	VariableDeclarationNode* SemanticAnalyzerVisitor::GetVariable(std::string_view identifier)
	{
		for (auto* variable : _variables_in_scope) {
			if (variable->name == identifier) {
				return variable;
			}
		}
		return nullptr;
	}
}  // namespace Soul::AST::Visitors
