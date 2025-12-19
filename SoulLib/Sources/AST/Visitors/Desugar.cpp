#include "AST/Visitors/Desugar.h"

#include <array>

namespace Soul::AST::Visitors
{
	using namespace Soul::Types;

	void DesugarVisitor::Visit(const BinaryNode& node)
	{
		CopyVisitor::Visit(node);

		// BinaryNode(lhs, rhs, complex_assign) => BinaryNode(lhs, BinarNode(lhs, rhs, desugared_op), Assign)
		static constexpr std::array k_complex_assignment_operators
			= { std::make_pair(ASTNode::Operator::ASSIGN_ADD, ASTNode::Operator::ADD),
			    std::make_pair(ASTNode::Operator::ASSIGN_SUB, ASTNode::Operator::SUB),
			    std::make_pair(ASTNode::Operator::ASSIGN_MUL, ASTNode::Operator::MUL),
			    std::make_pair(ASTNode::Operator::ASSIGN_DIV, ASTNode::Operator::DIV),
			    std::make_pair(ASTNode::Operator::ASSIGN_MOD, ASTNode::Operator::MOD) };
		const auto it{ std::ranges::find(
			k_complex_assignment_operators, node.op, &decltype(k_complex_assignment_operators)::value_type::first) };
		if (it != std::end(k_complex_assignment_operators)) {
			auto lhs_expression{ Clone(node.lhs.get()) };
			auto rhs_expression{ Clone(node.rhs.get()) };
			auto expression{ BinaryNode::Create(std::move(lhs_expression), std::move(rhs_expression), it->second) };
			expression->type = node.type;

			auto target{ Clone(node.lhs.get()) };
			_current_clone = BinaryNode::Create(std::move(target), std::move(expression), ASTNode::Operator::ASSIGN);
			_current_clone->type = node.type;
		}
	}  // namespace soul::ast::visitors

	void DesugarVisitor::Visit(const ForLoopNode& node)
	{
		// ForLoopNode(initialization, condition, update, BlockNode({...}))
		// ...is equivalent to...
		// BlockNode({initialization, WhileNode(condition, BlockNode({..., update}))})

		CopyVisitor::Visit(node);
		auto& for_loop = _current_clone->As<ForLoopNode>();

		// WhileNode(condition, BlockNode({..., update}))
		auto&& for_loop_statements = for_loop.statements->As<BlockNode>().statements;
		auto while_node_statements = ASTNode::Dependencies{};
		while_node_statements.reserve(for_loop_statements.size() + 1);
		while_node_statements.insert(while_node_statements.end(),
		                             std::make_move_iterator(for_loop_statements.begin()),
		                             std::make_move_iterator(for_loop_statements.end()));
		if (node.update) {
			while_node_statements.push_back(std::move(for_loop.update));
		}
		auto inner_scope  = BlockNode::Create(std::move(while_node_statements));
		inner_scope->type = for_loop.statements->type;

		auto while_node{ WhileNode::Create(std::move(for_loop.condition), std::move(inner_scope)) };
		while_node->type = node.type;

		// BlockNode({initialization, <while_node>})
		auto statements = ASTNode::Dependencies{};
		statements.reserve(2);
		if (for_loop.initialization) {
			statements.push_back(std::move(for_loop.initialization));
		}
		statements.push_back(std::move(while_node));

		_current_clone       = BlockNode::Create(std::move(statements));
		_current_clone->type = node.statements->type;
	}
}  // namespace Soul::AST::Visitors
