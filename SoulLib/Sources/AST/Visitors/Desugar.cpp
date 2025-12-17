#include "AST/Visitors/Desugar.h"

#include <array>

namespace Soul::AST::Visitors
{
	using namespace Soul::Types;

	void DesugarVisitor::visit(const BinaryNode& node)
	{
		CopyVisitor::visit(node);

		// BinaryNode(lhs, rhs, complex_assign) => BinaryNode(lhs, BinarNode(lhs, rhs, desugared_op), Assign)
		static constexpr std::array k_complex_assignment_operators
			= { std::make_pair(ASTNode::Operator::AddAssign, ASTNode::Operator::Add),
			    std::make_pair(ASTNode::Operator::SubAssign, ASTNode::Operator::Sub),
			    std::make_pair(ASTNode::Operator::MulAssign, ASTNode::Operator::Mul),
			    std::make_pair(ASTNode::Operator::DivAssign, ASTNode::Operator::Div),
			    std::make_pair(ASTNode::Operator::ModAssign, ASTNode::Operator::Mod) };
		const auto it{ std::ranges::find(
			k_complex_assignment_operators, node.op, &decltype(k_complex_assignment_operators)::value_type::first) };
		if (it != std::end(k_complex_assignment_operators)) {
			auto lhs_expression{ clone(node.lhs.get()) };
			auto rhs_expression{ clone(node.rhs.get()) };
			auto expression{ BinaryNode::create(std::move(lhs_expression), std::move(rhs_expression), it->second) };
			expression->type = node.type;

			auto target{ clone(node.lhs.get()) };
			_current_clone = BinaryNode::create(std::move(target), std::move(expression), ASTNode::Operator::Assign);
			_current_clone->type = node.type;
		}
	}  // namespace soul::ast::visitors

	void DesugarVisitor::visit(const ForLoopNode& node)
	{
		// ForLoopNode(initialization, condition, update, BlockNode({...}))
		// ...is equivalent to...
		// BlockNode({initialization, WhileNode(condition, BlockNode({..., update}))})

		CopyVisitor::visit(node);
		auto& for_loop = _current_clone->as<ForLoopNode>();

		// WhileNode(condition, BlockNode({..., update}))
		auto&& for_loop_statements = for_loop.statements->as<BlockNode>().statements;
		auto while_node_statements = ASTNode::Dependencies{};
		while_node_statements.reserve(for_loop_statements.size() + 1);
		while_node_statements.insert(while_node_statements.end(),
		                             std::make_move_iterator(for_loop_statements.begin()),
		                             std::make_move_iterator(for_loop_statements.end()));
		if (node.update) {
			while_node_statements.push_back(std::move(for_loop.update));
		}
		auto inner_scope  = BlockNode::create(std::move(while_node_statements));
		inner_scope->type = for_loop.statements->type;

		auto while_node{ WhileNode::create(std::move(for_loop.condition), std::move(inner_scope)) };
		while_node->type = node.type;

		// BlockNode({initialization, <while_node>})
		auto statements = ASTNode::Dependencies{};
		statements.reserve(2);
		if (for_loop.initialization) {
			statements.push_back(std::move(for_loop.initialization));
		}
		statements.push_back(std::move(while_node));

		_current_clone       = BlockNode::create(std::move(statements));
		_current_clone->type = node.statements->type;
	}
}  // namespace Soul::AST::Visitors
