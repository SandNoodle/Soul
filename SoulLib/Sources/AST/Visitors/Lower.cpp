#include "AST/Visitors/Lower.h"

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
			std::vector<std::pair<std::string, Type>> required_slots;

			public:
			using DefaultTraverseVisitor::Accept;

			protected:
			using DefaultTraverseVisitor::Visit;
			void Visit(const VariableDeclarationNode& node);
		};

		void ReserveStackSlotVisitor::Visit(const VariableDeclarationNode& node)
		{
			required_slots.emplace_back(std::make_pair(node.name, node.type));
		}
	}  // namespace Detail

	std::unique_ptr<Module> LowerVisitor::Get() noexcept { return _builder.Build(); }

	void LowerVisitor::Visit(const BinaryNode& node) { _current_instruction = Emit(node); }
	void LowerVisitor::Visit(const BlockNode& node)
	{
		auto* output_block = _builder.CreateBasicBlock();
		_builder.Emit<Jump>(output_block);
		_builder.SwitchTo(output_block);
		for (const auto& statement : node.statements) {
			Accept(statement.get());
		}
	}
	void LowerVisitor::Visit(const CastNode& node) { _current_instruction = Emit(node); }
	void LowerVisitor::Visit(const ErrorNode& node) { _current_instruction = Emit(node); }

	void LowerVisitor::Visit(const ForLoopNode& node)
	{
		auto* condition_block = _builder.CreateBasicBlock();
		auto* body_block      = _builder.CreateBasicBlock();
		auto* update_block    = _builder.CreateBasicBlock();
		auto* output_block    = _builder.CreateBasicBlock();

		_loop_jump_targets.emplace_back(std::make_pair(update_block, output_block));

		Emit(node.initialization.get());
		_builder.Emit<Jump>(condition_block);

		_builder.SwitchTo(condition_block);
		auto* condition = Emit(node.condition.get());
		_builder.Emit<JumpIf>(condition, body_block, output_block);

		_builder.SwitchTo(body_block);
		for (const auto& statement : node.statements->As<BlockNode>().statements) {
			Accept(statement.get());
		}
		_builder.Emit<Jump>(update_block);

		_builder.SwitchTo(update_block);
		Emit(node.update.get());
		_builder.Emit<Jump>(condition_block);

		_builder.SwitchTo(output_block);
		_loop_jump_targets.pop_back();
	}

	void LowerVisitor::Visit(const ForeachLoopNode& node) { _current_instruction = Emit(node); }
	void LowerVisitor::Visit(const FunctionCallNode& node) { _current_instruction = Emit(node); }

	void LowerVisitor::Visit(const FunctionDeclarationNode& node)
	{
		std::vector<Types::Type> parameters{};
		parameters.reserve(node.parameters.size());
		for (const auto& parameter : node.parameters) {
			parameters.emplace_back(parameter->type);
		}

		_builder.CreateFunction(node.name, node.type, std::move(parameters));

		// NOTE: Traverse the tree while gathering all variable declarations and reserve stack slots for them.
		Detail::ReserveStackSlotVisitor reserve_stack_slot_visitor{};
		reserve_stack_slot_visitor.Accept(&static_cast<ASTNode&>(const_cast<FunctionDeclarationNode&>(node)));
		for (const auto& [slot_name, slot_type] : reserve_stack_slot_visitor.required_slots) {
			std::ignore = _builder.ReserveSlot(slot_name, slot_type);
		}

		for (std::size_t index = 0; index < node.parameters.size(); ++index) {
			// NOTE: In the case of parameters, even though function parameters are always VariableDeclarations, they
			// don't bind to any known values (which are set on the call site) - thus require special handling.
			const auto& parameter = node.parameters[index]->As<VariableDeclarationNode>();

			auto* slot{ _builder.GetSlot(parameter.name) };
			auto* value{ _builder.Emit<GetArgument>(parameter.type, index) };
			_builder.Emit<StackStore>(slot, value);
		}
		for (const auto& statement : node.statements->As<BlockNode>().statements) {
			Accept(statement.get());
		}
	}

	void LowerVisitor::Visit(const IfNode& node)
	{
		auto* input_block  = _builder.CurrentBasicBlock();
		auto* then_block   = _builder.CreateBasicBlock();
		auto* else_block   = _builder.CreateBasicBlock();
		auto* output_block = _builder.CreateBasicBlock();

		_builder.SwitchTo(input_block);
		_builder.Emit<JumpIf>(Emit(node.condition.get()), then_block, else_block);

		_builder.SwitchTo(then_block);
		for (const auto& statement : node.then_statements->As<BlockNode>().statements) {
			Accept(statement.get());
		}
		_builder.Emit<Jump>(output_block);

		_builder.SwitchTo(else_block);
		for (const auto& statement : node.else_statements->As<BlockNode>().statements) {
			Accept(statement.get());
		}
		_builder.Emit<Jump>(output_block);

		_builder.SwitchTo(output_block);
	}

	void LowerVisitor::Visit(const LiteralNode& node) { _current_instruction = Emit(node); }
	void LowerVisitor::Visit(const LoopControlNode& node) { _current_instruction = Emit(node); }

	void LowerVisitor::Visit(const ModuleNode& node)
	{
		_builder.SetModuleName(node.name);
		// NOTE: At the top level only FunctionDeclarationNodes should remain (and be visited).
		for (const auto& statement : node.statements | std::views::filter([](const auto& e) -> bool {
										 return e->template Is<FunctionDeclarationNode>();
									 })) {
			Accept(statement.get());
		}
	}

	void LowerVisitor::Visit(const ReturnNode& node) { _current_instruction = Emit(node); }
	void LowerVisitor::Visit(const StructDeclarationNode& node) { _current_instruction = Emit(node); }
	void LowerVisitor::Visit(const UnaryNode& node) { _current_instruction = Emit(node); }
	void LowerVisitor::Visit(const VariableDeclarationNode& node) { _current_instruction = Emit(node); }

	void LowerVisitor::Visit(const WhileNode& node)
	{
		auto* condition_block = _builder.CreateBasicBlock();
		_builder.Emit<Jump>(condition_block);

		_builder.SwitchTo(condition_block);
		auto* body_block   = _builder.CreateBasicBlock();
		auto* output_block = _builder.CreateBasicBlock();
		_builder.Emit<JumpIf>(Emit(node.condition.get()), body_block, output_block);

		_builder.SwitchTo(body_block);
		_loop_jump_targets.emplace_back(std::make_pair(condition_block, output_block));
		for (const auto& statement : node.statements->As<BlockNode>().statements) {
			Accept(statement.get());
		}
		_builder.Emit<Jump>(condition_block);
		_loop_jump_targets.pop_back();

		_builder.SwitchTo(output_block);
	}

	Instruction* LowerVisitor::Emit(const ASTNode::Reference node)
	{
		if (!node) {
			return nullptr;
		}
		node->Accept(*this);
		return _current_instruction;
	}

	Instruction* LowerVisitor::Emit(const BinaryNode& node)
	{
		// IMPORTANT: Assignment operator is the only one which can change the meaning of LiteralNode(Identifier)s
		// depending on which side it is present.
		if (node.op == ASTNode::Operator::ASSIGN) {
			// 1. For the LHS, if Identifier is present, this will mean the value its pointing to is being overridden.
			const bool is_write_target
				= node.lhs->Is<LiteralNode>() && node.lhs->As<LiteralNode>().value.Is<Identifier>();
			if (is_write_target) {
				auto* slot{ _builder.GetSlot(node.lhs->As<LiteralNode>().value.As<Identifier>()) };
				auto* value{ Emit(node.rhs.get()) };
				return _builder.Emit<StackStore>(slot, value);
			}

			// 2. For the RHS, we can (safely) assume that all operations will be READ operations.
			return Emit(node.rhs.get());
		}

		auto* lhs{ Emit(node.lhs.get()) };
		auto* rhs{ Emit(node.rhs.get()) };

		switch (node.op) {
			case ASTNode::Operator::ADD:
				return _builder.Emit<Add>(node.type, lhs, rhs);
			case ASTNode::Operator::SUB:
				return _builder.Emit<Sub>(node.type, lhs, rhs);
			case ASTNode::Operator::MUL:
				return _builder.Emit<Mul>(node.type, lhs, rhs);
			case ASTNode::Operator::DIV:
				return _builder.Emit<Div>(node.type, lhs, rhs);
			case ASTNode::Operator::MOD:
				return _builder.Emit<Mod>(node.type, lhs, rhs);
			case ASTNode::Operator::EQUAL:
				return _builder.Emit<Equal>(lhs, rhs);
			case ASTNode::Operator::NOT_EQUAL:
				return _builder.Emit<NotEqual>(lhs, rhs);
			case ASTNode::Operator::GREATER:
				return _builder.Emit<Greater>(lhs, rhs);
			case ASTNode::Operator::GREATER_EQUAL:
				return _builder.Emit<GreaterEqual>(lhs, rhs);
			case ASTNode::Operator::LESS:
				return _builder.Emit<Less>(lhs, rhs);
			case ASTNode::Operator::LESS_EQUAL:
				return _builder.Emit<LessEqual>(lhs, rhs);
			case ASTNode::Operator::LOGICAL_AND:
				return _builder.Emit<And>(lhs, rhs);
			case ASTNode::Operator::LOGICAL_OR:
				return _builder.Emit<Or>(lhs, rhs);
			default:
				break;
		}

		// ERROR: unhandled case OR non-desugared instruction present.
		return _builder.Emit<Unreachable>();
	}

	Instruction* LowerVisitor::Emit(const BlockNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.Emit<Unreachable>();
	}

	Instruction* LowerVisitor::Emit(const CastNode& node)
	{
		return _builder.Emit<Cast>(node.type, Emit(node.expression.get()));
	}

	Instruction* LowerVisitor::Emit(const ErrorNode&)
	{
		// ERROR: AST should be in a valid state at this point.
		return _builder.Emit<Unreachable>();
	}

	Instruction* LowerVisitor::Emit(const ForLoopNode&)
	{
		// ERROR: should've been replaced with WhileNode at this point.
		return _builder.Emit<Unreachable>();
	}

	Instruction* LowerVisitor::Emit(const ForeachLoopNode&)
	{
		// ERROR: should've been replaced with ForLoop node at this point.
		return _builder.Emit<Unreachable>();
	}

	Instruction* LowerVisitor::Emit(const FunctionCallNode& node)
	{
		std::vector<Instruction*> parameters;
		parameters.reserve(node.parameters.size());
		for (const auto& parameter : node.parameters) {
			parameters.emplace_back(Emit(parameter.get()));
		}
		return _builder.Emit<Call>(node.type, node.name, std::move(parameters));
	}

	Instruction* LowerVisitor::Emit(const FunctionDeclarationNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.Emit<Unreachable>();
	}

	Instruction* LowerVisitor::Emit(const IfNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.Emit<Unreachable>();
	}

	Instruction* LowerVisitor::Emit(const LiteralNode& node)
	{
		// IMPORTANT: We assume that visiting LiteralNode will always result in a READ operation for Identifiers, as
		// any special (i.e. writing) logic will be handled beforehand.
		if (node.value.Is<Identifier>()) {
			auto* slot = _builder.GetSlot(node.value.As<Identifier>());
			return _builder.Emit<StackLoad>(slot);
		}
		return _builder.Emit<Const>(node.type, node.value);
	}

	Instruction* LowerVisitor::Emit(const LoopControlNode& node)
	{
		if (_loop_jump_targets.empty()) [[unlikely]] {
			return _builder.Emit<Unreachable>();
		}

		const auto& [continuation_block, termination_block] = _loop_jump_targets.back();
		if (node.control_type == LoopControlNode::Type::BREAK) {
			return _builder.Emit<Jump>(termination_block);
		}
		if (node.control_type == LoopControlNode::Type::CONTINUE) {
			return _builder.Emit<Jump>(continuation_block);
		}
		// NOTE: unhandled case
		return _builder.Emit<Unreachable>();
	}

	Instruction* LowerVisitor::Emit(const ModuleNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.Emit<Unreachable>();
	}

	Instruction* LowerVisitor::Emit(const ReturnNode& node)
	{
		auto* expression{ Emit(node.expression.get()) };
		return _builder.Emit<Return>(expression);
	}

	Instruction* LowerVisitor::Emit(const StructDeclarationNode&)
	{
		// NOTE: ERROR; at this point this node should not be present.
		return _builder.Emit<Unreachable>();
	}

	Instruction* LowerVisitor::Emit(const UnaryNode& node)
	{
		auto* expression{ Emit(node.expression.get()) };
		switch (node.op) {
			case ASTNode::Operator::LOGICAL_NOT:
				return _builder.Emit<Not>(expression);
			default:
				break;
		}
		// ERROR: unhandled case OR non-desugared instruction present.
		return _builder.Emit<Unreachable>();
	}

	Instruction* LowerVisitor::Emit(const VariableDeclarationNode& node)
	{
		auto* slot{ _builder.GetSlot(node.name) };
		auto* value{ Emit(node.expression.get()) };
		return _builder.Emit<StackStore>(slot, value);
	}

	Instruction* LowerVisitor::Emit(const WhileNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.Emit<Unreachable>();
	}
}  // namespace Soul::AST::Visitors
