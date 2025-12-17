#include "ast/visitors/semantic_analyzer.h"

#include "ast/ast.h"

namespace soul::ast::visitors
{
	void SemanticAnalyzerVisitor::visit(const BinaryNode& node)
	{
		CopyVisitor::visit(node);

		const auto& binary_node = _current_clone->as<BinaryNode>();
		if (binary_node.op == ASTNode::Operator::Assign && binary_node.lhs->is<LiteralNode>()) {
			const auto& literal = binary_node.lhs->as<LiteralNode>();
			if (literal.value.is<Identifier>()) {
				const auto& identifier            = std::string(literal.value.as<Identifier>());
				VariableDeclarationNode* variable = get_variable(identifier);
				if (!variable) {
					_current_clone = ErrorNode::create(
						std::format("[INTERNAL] identifier '{}' should've been checked at this point.", identifier));
					return;
				}
				if (!variable->is_mutable) {
					_current_clone = ErrorNode::create(
						std::format("cannot assign to variable '{}', because it is not mutable.", identifier));
					return;
				}
			}
		}
	}

	void SemanticAnalyzerVisitor::visit(const BlockNode& node)
	{
		// ENTER SCOPE: Mark a restorepoint for variables declared in the current scope.
		_current_depth++;
		const std::size_t variables_until_this_point = _variables_in_scope.size();

		CopyVisitor::visit(node);
		// EXIT SCOPE: Remove variables defined in that scope.
		const std::size_t variables_declared_in_scope = _variables_in_scope.size() - variables_until_this_point;
		_variables_in_scope.erase(_variables_in_scope.end() - static_cast<std::ptrdiff_t>(variables_declared_in_scope),
		                          _variables_in_scope.end());
		_current_depth--;
	}

	void SemanticAnalyzerVisitor::visit(const ForLoopNode& node)
	{
		const bool was_in_loop = _is_in_loop;
		_is_in_loop            = true;
		CopyVisitor::visit(node);
		_is_in_loop = was_in_loop;
	}

	void SemanticAnalyzerVisitor::visit(const ForeachLoopNode& node)
	{
		const bool was_in_loop = _is_in_loop;
		_is_in_loop            = true;
		CopyVisitor::visit(node);
		_is_in_loop = was_in_loop;
	}

	void SemanticAnalyzerVisitor::visit(const FunctionDeclarationNode& node)
	{
		_variables_in_scope.clear();
		CopyVisitor::visit(node);
	}

	void SemanticAnalyzerVisitor::visit(const LiteralNode& node)
	{
		if (node.value.is<Identifier>()) {
			const auto& identifier            = node.value.as<Identifier>();
			VariableDeclarationNode* variable = get_variable(identifier);
			if (!variable) {
				_current_clone
					= ErrorNode::create(std::format("use of an undeclared identifier '{}'", std::string(identifier)));

				return;
			}
		}
		CopyVisitor::visit(node);
	}

	void SemanticAnalyzerVisitor::visit(const LoopControlNode& node)
	{
		if (!_is_in_loop) {
			_current_clone = ErrorNode::create(
				std::format("keyword '{}' must be used in a loop context.",
			                node.control_type == LoopControlNode::Type::Break ? "break" : "continue"));
			return;
		}
		CopyVisitor::visit(node);
	}

	void SemanticAnalyzerVisitor::visit(const VariableDeclarationNode& node)
	{
		if (_current_depth == 0) {
			_current_clone
				= ErrorNode::create(std::format("variable '{}' cannot be declared in the global scope.", node.name));
			return;
		}
		CopyVisitor::visit(node);
		_variables_in_scope.emplace_back(&_current_clone->as<VariableDeclarationNode>());
	}

	void SemanticAnalyzerVisitor::visit(const WhileNode& node)
	{
		const bool was_in_loop = _is_in_loop;
		_is_in_loop            = true;
		CopyVisitor::visit(node);
		_is_in_loop = was_in_loop;
	}

	VariableDeclarationNode* SemanticAnalyzerVisitor::get_variable(std::string_view identifier)
	{
		for (auto* variable : _variables_in_scope) {
			if (variable->name == identifier) {
				return variable;
			}
		}
		return nullptr;
	}
}  // namespace soul::ast::visitors
