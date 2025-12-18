#include "AST/Visitors/Lower.h"

#include <array>
#include <ranges>

namespace Soul::AST::Visitors
{
	using namespace Soul::Types;
	using namespace Soul::IR;

	namespace Detail
	{
		class ReserveStackSlotVisitor : public DefaultTraverseVisitor
		{
			public:
			std::vector<std::pair<std::string, Types::Type>> required_slots;

			public:
			using DefaultTraverseVisitor::accept;

			protected:
			using DefaultTraverseVisitor::visit;
			void visit(const VariableDeclarationNode& node);
		};

		void ReserveStackSlotVisitor::visit(const VariableDeclarationNode& node)
		{
			required_slots.emplace_back(std::make_pair(node.name, node.type));
		}
	}  // namespace Detail

	std::unique_ptr<Module> LowerVisitor::get() noexcept { return _builder.Build(); }

	void LowerVisitor::visit(const BinaryNode& node) { _current_instruction = emit(node); }
	void LowerVisitor::visit(const BlockNode& node)
	{
		auto* output_block = _builder.CreateBasicBlock();
		_builder.Emit<Jump>(output_block);
		_builder.SwitchTo(output_block);
		for (const auto& statement : node.statements) {
			accept(statement.get());
		}
	}
	void LowerVisitor::visit(const CastNode& node) { _current_instruction = emit(node); }
	void LowerVisitor::visit(const ErrorNode& node) { _current_instruction = emit(node); }

	void LowerVisitor::visit(const ForLoopNode& node)
	{
		auto* condition_block = _builder.CreateBasicBlock();
		auto* body_block      = _builder.CreateBasicBlock();
		auto* update_block    = _builder.CreateBasicBlock();
		auto* output_block    = _builder.CreateBasicBlock();

		_loop_jump_targets.emplace_back(std::make_pair(update_block, output_block));

		emit(node.initialization.get());
		_builder.Emit<Jump>(condition_block);

		_builder.SwitchTo(condition_block);
		auto* condition = emit(node.condition.get());
		_builder.Emit<JumpIf>(condition, body_block, output_block);

		_builder.SwitchTo(body_block);
		for (const auto& statement : node.statements->as<BlockNode>().statements) {
			accept(statement.get());
		}
		_builder.Emit<Jump>(update_block);

		_builder.SwitchTo(update_block);
		emit(node.update.get());
		_builder.Emit<Jump>(condition_block);

		_builder.SwitchTo(output_block);
		_loop_jump_targets.pop_back();
	}

	void LowerVisitor::visit(const ForeachLoopNode& node) { _current_instruction = emit(node); }
	void LowerVisitor::visit(const FunctionCallNode& node) { _current_instruction = emit(node); }

	void LowerVisitor::visit(const FunctionDeclarationNode& node)
	{
		std::vector<Types::Type> parameters{};
		parameters.reserve(node.parameters.size());
		for (const auto& parameter : node.parameters) {
			parameters.emplace_back(parameter->type);
		}

		_builder.CreateFunction(node.name, node.type, std::move(parameters));

		// NOTE: Traverse the tree while gathering all variable declarations and reserve stack slots for them.
		Detail::ReserveStackSlotVisitor reserve_stack_slot_visitor{};
		reserve_stack_slot_visitor.accept(&static_cast<ASTNode&>(const_cast<FunctionDeclarationNode&>(node)));
		for (const auto& [slot_name, slot_type] : reserve_stack_slot_visitor.required_slots) {
			std::ignore = _builder.ReserveSlot(slot_name, slot_type);
		}

		for (std::size_t index = 0; index < node.parameters.size(); ++index) {
			// NOTE: In the case of parameters, even though function parameters are always VariableDeclarations, they
			// don't bind to any known values (which are set on the call site) - thus require special handling.
			const auto& parameter = node.parameters[index]->as<VariableDeclarationNode>();

			auto* slot{ _builder.GetSlot(parameter.name) };
			auto* value{ _builder.Emit<GetArgument>(parameter.type, index) };
			_builder.Emit<StackStore>(slot, value);
		}
		for (const auto& statement : node.statements->as<BlockNode>().statements) {
			accept(statement.get());
		}
	}

	void LowerVisitor::visit(const IfNode& node)
	{
		auto* input_block  = _builder.CurrentBasicBlock();
		auto* then_block   = _builder.CreateBasicBlock();
		auto* else_block   = _builder.CreateBasicBlock();
		auto* output_block = _builder.CreateBasicBlock();

		_builder.SwitchTo(input_block);
		_builder.Emit<JumpIf>(emit(node.condition.get()), then_block, else_block);

		_builder.SwitchTo(then_block);
		for (const auto& statement : node.then_statements->as<BlockNode>().statements) {
			accept(statement.get());
		}
		_builder.Emit<Jump>(output_block);

		_builder.SwitchTo(else_block);
		for (const auto& statement : node.else_statements->as<BlockNode>().statements) {
			accept(statement.get());
		}
		_builder.Emit<Jump>(output_block);

		_builder.SwitchTo(output_block);
	}

	void LowerVisitor::visit(const LiteralNode& node) { _current_instruction = emit(node); }
	void LowerVisitor::visit(const LoopControlNode& node) { _current_instruction = emit(node); }

	void LowerVisitor::visit(const ModuleNode& node)
	{
		_builder.SetModuleName(node.name);
		// NOTE: At the top level only FunctionDeclarationNodes should remain (and be visited).
		for (const auto& statement : node.statements | std::views::filter([](const auto& e) -> bool {
										 return e->template is<FunctionDeclarationNode>();
									 })) {
			accept(statement.get());
		}
	}

	void LowerVisitor::visit(const ReturnNode& node) { _current_instruction = emit(node); }
	void LowerVisitor::visit(const StructDeclarationNode& node) { _current_instruction = emit(node); }
	void LowerVisitor::visit(const UnaryNode& node) { _current_instruction = emit(node); }
	void LowerVisitor::visit(const VariableDeclarationNode& node) { _current_instruction = emit(node); }

	void LowerVisitor::visit(const WhileNode& node)
	{
		auto* condition_block = _builder.CreateBasicBlock();
		_builder.Emit<Jump>(condition_block);

		_builder.SwitchTo(condition_block);
		auto* body_block   = _builder.CreateBasicBlock();
		auto* output_block = _builder.CreateBasicBlock();
		_builder.Emit<JumpIf>(emit(node.condition.get()), body_block, output_block);

		_builder.SwitchTo(body_block);
		_loop_jump_targets.emplace_back(std::make_pair(condition_block, output_block));
		for (const auto& statement : node.statements->as<BlockNode>().statements) {
			accept(statement.get());
		}
		_builder.Emit<Jump>(condition_block);
		_loop_jump_targets.pop_back();

		_builder.SwitchTo(output_block);
	}

	Instruction* LowerVisitor::emit(const ASTNode::Reference node)
	{
		if (!node) {
			return nullptr;
		}
		node->accept(*this);
		return _current_instruction;
	}

	Instruction* LowerVisitor::emit(const BinaryNode& node)
	{
		// IMPORTANT: Assignment operator is the only one which can change the meaning of LiteralNode(Identifier)s
		// depending on which side it is present.
		if (node.op == ASTNode::Operator::Assign) {
			// 1. For the LHS, if Identifier is present, this will mean the value its pointing to is being overridden.
			const bool is_write_target
				= node.lhs->is<LiteralNode>() && node.lhs->as<LiteralNode>().value.Is<Identifier>();
			if (is_write_target) {
				auto* slot{ _builder.GetSlot(node.lhs->as<LiteralNode>().value.As<Identifier>()) };
				auto* value{ emit(node.rhs.get()) };
				return _builder.Emit<StackStore>(slot, value);
			}

			// 2. For the RHS, we can (safely) assume that all operations will be READ operations.
			return emit(node.rhs.get());
		}

		auto* lhs{ emit(node.lhs.get()) };
		auto* rhs{ emit(node.rhs.get()) };

		switch (node.op) {
			case ASTNode::Operator::Add:
				return _builder.Emit<Add>(node.type, lhs, rhs);
			case ASTNode::Operator::Sub:
				return _builder.Emit<Sub>(node.type, lhs, rhs);
			case ASTNode::Operator::Mul:
				return _builder.Emit<Mul>(node.type, lhs, rhs);
			case ASTNode::Operator::Div:
				return _builder.Emit<Div>(node.type, lhs, rhs);
			case ASTNode::Operator::Mod:
				return _builder.Emit<Mod>(node.type, lhs, rhs);
			case ASTNode::Operator::Equal:
				return _builder.Emit<Equal>(lhs, rhs);
			case ASTNode::Operator::NotEqual:
				return _builder.Emit<NotEqual>(lhs, rhs);
			case ASTNode::Operator::Greater:
				return _builder.Emit<Greater>(lhs, rhs);
			case ASTNode::Operator::GreaterEqual:
				return _builder.Emit<GreaterEqual>(lhs, rhs);
			case ASTNode::Operator::Less:
				return _builder.Emit<Less>(lhs, rhs);
			case ASTNode::Operator::LessEqual:
				return _builder.Emit<LessEqual>(lhs, rhs);
			case ASTNode::Operator::LogicalAnd:
				return _builder.Emit<And>(lhs, rhs);
			case ASTNode::Operator::LogicalOr:
				return _builder.Emit<Or>(lhs, rhs);
			default:
				break;
		}

		// ERROR: unhandled case OR non-desugared instruction present.
		return _builder.Emit<Unreachable>();
	}

	IR::Instruction* LowerVisitor::emit(const BlockNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.Emit<Unreachable>();
	}

	IR::Instruction* LowerVisitor::emit(const CastNode& node)
	{
		return _builder.Emit<Cast>(node.type, emit(node.expression.get()));
	}

	IR::Instruction* LowerVisitor::emit(const ErrorNode&)
	{
		// ERROR: AST should be in a valid state at this point.
		return _builder.Emit<Unreachable>();
	}

	IR::Instruction* LowerVisitor::emit(const ForLoopNode&)
	{
		// ERROR: should've been replaced with WhileNode at this point.
		return _builder.Emit<Unreachable>();
	}

	IR::Instruction* LowerVisitor::emit(const ForeachLoopNode&)
	{
		// ERROR: should've been replaced with ForLoop node at this point.
		return _builder.Emit<Unreachable>();
	}

	IR::Instruction* LowerVisitor::emit(const FunctionCallNode& node)
	{
		std::vector<Instruction*> parameters;
		parameters.reserve(node.parameters.size());
		for (const auto& parameter : node.parameters) {
			parameters.emplace_back(emit(parameter.get()));
		}
		return _builder.Emit<Call>(node.type, node.name, std::move(parameters));
	}

	IR::Instruction* LowerVisitor::emit(const FunctionDeclarationNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.Emit<Unreachable>();
	}

	IR::Instruction* LowerVisitor::emit(const IfNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.Emit<Unreachable>();
	}

	IR::Instruction* LowerVisitor::emit(const LiteralNode& node)
	{
		// IMPORTANT: We assume that visiting LiteralNode will always result in a READ operation for Identifiers, as
		// any special (i.e. writing) logic will be handled beforehand.
		if (node.value.Is<Identifier>()) {
			auto* slot = _builder.GetSlot(node.value.As<Identifier>());
			return _builder.Emit<StackLoad>(slot);
		}
		return _builder.Emit<Const>(node.type, node.value);
	}

	IR::Instruction* LowerVisitor::emit(const LoopControlNode& node)
	{
		if (_loop_jump_targets.empty()) [[unlikely]] {
			return _builder.Emit<Unreachable>();
		}

		const auto& [continuation_block, termination_block] = _loop_jump_targets.back();
		auto* current_block                                 = _builder.CurrentBasicBlock();
		if (node.control_type == LoopControlNode::Type::Break) {
			return _builder.Emit<Jump>(termination_block);
		}
		if (node.control_type == LoopControlNode::Type::Continue) {
			return _builder.Emit<Jump>(continuation_block);
		}
		// NOTE: unhandled case
		return _builder.Emit<Unreachable>();
	}

	IR::Instruction* LowerVisitor::emit(const ModuleNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.Emit<Unreachable>();
	}

	IR::Instruction* LowerVisitor::emit(const ReturnNode& node)
	{
		auto* expression{ emit(node.expression.get()) };
		return _builder.Emit<Return>(expression);
	}

	IR::Instruction* LowerVisitor::emit(const StructDeclarationNode&)
	{
		// NOTE: ERROR; at this point this node should not be present.
		return _builder.Emit<Unreachable>();
	}

	IR::Instruction* LowerVisitor::emit(const UnaryNode& node)
	{
		auto* expression{ emit(node.expression.get()) };
		switch (node.op) {
			case ASTNode::Operator::LogicalNot:
				return _builder.Emit<Not>(expression);
			default:
				break;
		}
		// ERROR: unhandled case OR non-desugared instruction present.
		return _builder.Emit<Unreachable>();
	}

	IR::Instruction* LowerVisitor::emit(const VariableDeclarationNode& node)
	{
		auto* slot{ _builder.GetSlot(node.name) };
		auto* value{ emit(node.expression.get()) };
		return _builder.Emit<StackStore>(slot, value);
	}

	IR::Instruction* LowerVisitor::emit(const WhileNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.Emit<Unreachable>();
	}
}  // namespace Soul::AST::Visitors
