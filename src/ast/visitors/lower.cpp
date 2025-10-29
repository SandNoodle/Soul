#include "ast/visitors/lower.h"

#include <array>
#include <ranges>

namespace soul::ast::visitors
{
	using namespace soul::types;
	using namespace soul::ir;

	namespace detail
	{
		class ReserveStackSlotVisitor : public DefaultTraverseVisitor
		{
			public:
			std::vector<std::pair<std::string, types::Type>> required_slots;

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
	}  // namespace detail

	std::unique_ptr<Module> LowerVisitor::get() noexcept { return _builder.build(); }

	void LowerVisitor::visit(const BinaryNode& node) { _current_instruction = emit(node); }
	void LowerVisitor::visit(const BlockNode& node)
	{
		auto* output_block = _builder.create_basic_block();
		_builder.connect(_builder.current_basic_block(), output_block);
		_builder.emit<Jump>(output_block);
		_builder.switch_to(output_block);
		for (const auto& statement : node.statements) {
			accept(statement.get());
		}
	}
	void LowerVisitor::visit(const CastNode& node) { _current_instruction = emit(node); }
	void LowerVisitor::visit(const ErrorNode& node) { _current_instruction = emit(node); }

	void LowerVisitor::visit(const ForLoopNode& node)
	{
		auto* input_block     = _builder.current_basic_block();
		auto* condition_block = _builder.create_basic_block();
		auto* body_block      = _builder.create_basic_block();
		auto* update_block    = _builder.create_basic_block();
		auto* output_block    = _builder.create_basic_block();

		_builder.connect(std::array{ input_block, update_block }, condition_block);
		_builder.connect(condition_block, std::array{ body_block, output_block });
		_builder.connect(body_block, update_block);

		emit(node.initialization.get());
		_builder.emit<Jump>(condition_block);

		_builder.switch_to(condition_block);
		auto* condition = emit(node.condition.get());
		_builder.emit<JumpIf>(condition, body_block, output_block);

		_builder.switch_to(body_block);
		for (const auto& statement : node.statements->as<BlockNode>().statements) {
			accept(statement.get());
		}
		_builder.emit<Jump>(update_block);

		_builder.switch_to(update_block);
		emit(node.update.get());
		_builder.emit<Jump>(condition_block);

		_builder.switch_to(output_block);
	}

	void LowerVisitor::visit(const ForeachLoopNode& node) { _current_instruction = emit(node); }
	void LowerVisitor::visit(const FunctionCallNode& node) { _current_instruction = emit(node); }

	void LowerVisitor::visit(const FunctionDeclarationNode& node)
	{
		std::vector<types::Type> parameters{};
		parameters.reserve(node.parameters.size());
		for (const auto& parameter : node.parameters) {
			parameters.emplace_back(parameter->type);
		}

		_builder.create_function(node.name, node.type, std::move(parameters));

		// NOTE: Traverse the tree while gathering all variable declarations and reserve stack slots for them.
		detail::ReserveStackSlotVisitor reserve_stack_slot_visitor{};
		reserve_stack_slot_visitor.accept(&static_cast<ASTNode&>(const_cast<FunctionDeclarationNode&>(node)));
		for (const auto& [slot_name, slot_type] : reserve_stack_slot_visitor.required_slots) {
			std::ignore = _builder.reserve_slot(slot_name, slot_type);
		}

		for (std::size_t index = 0; index < node.parameters.size(); ++index) {
			// NOTE: In the case of parameters, even though function parameters are always VariableDeclarations, they
			// don't bind to any known values (which are set on the call site) - thus require special handling.
			const auto& parameter = node.parameters[index]->as<VariableDeclarationNode>();

			auto* slot{ _builder.get_slot(parameter.name) };
			auto* value{ _builder.emit<GetArgument>(parameter.type, index) };
			_builder.emit<StackStore>(slot, value);
		}
		for (const auto& statement : node.statements->as<BlockNode>().statements) {
			accept(statement.get());
		}
	}

	void LowerVisitor::visit(const IfNode& node)
	{
		auto* input_block  = _builder.current_basic_block();
		auto* then_block   = _builder.create_basic_block();
		auto* else_block   = _builder.create_basic_block();
		auto* output_block = _builder.create_basic_block();

		_builder.connect(input_block, std::array{ then_block, else_block });
		_builder.connect(std::array{ then_block, else_block }, output_block);

		_builder.switch_to(input_block);
		_builder.emit<JumpIf>(emit(node.condition.get()), then_block, else_block);

		_builder.switch_to(then_block);
		for (const auto& statement : node.then_statements->as<BlockNode>().statements) {
			accept(statement.get());
		}
		_builder.emit<Jump>(output_block);

		_builder.switch_to(else_block);
		for (const auto& statement : node.else_statements->as<BlockNode>().statements) {
			accept(statement.get());
		}
		_builder.emit<Jump>(output_block);

		_builder.switch_to(output_block);
	}

	void LowerVisitor::visit(const LiteralNode& node) { _current_instruction = emit(node); }

	void LowerVisitor::visit(const ModuleNode& node)
	{
		_builder.set_module_name(node.name);
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
		auto* input_block     = _builder.current_basic_block();
		auto* condition_block = _builder.create_basic_block();
		auto* body_block      = _builder.create_basic_block();
		auto* output_block    = _builder.create_basic_block();

		_builder.connect(input_block, condition_block);
		_builder.connect(condition_block, std::array{ body_block, output_block });
		_builder.connect(body_block, condition_block);

		_builder.switch_to(input_block);
		_builder.emit<Jump>(condition_block);

		_builder.switch_to(condition_block);
		_builder.emit<JumpIf>(emit(node.condition.get()), body_block, output_block);

		_builder.switch_to(body_block);
		for (const auto& statement : node.statements->as<BlockNode>().statements) {
			accept(statement.get());
		}
		_builder.emit<Jump>(condition_block);

		_builder.switch_to(output_block);
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
				= node.lhs->is<LiteralNode>() && node.lhs->as<LiteralNode>().value.is<Identifier>();
			if (is_write_target) {
				auto* slot{ _builder.get_slot(node.lhs->as<LiteralNode>().value.as<Identifier>()) };
				auto* value{ emit(node.rhs.get()) };
				return _builder.emit<StackStore>(slot, value);
			}

			// 2. For the RHS, we can (safely) assume that all operations will be a READ operation.
			return emit(node.rhs.get());
		}

		auto* lhs{ emit(node.lhs.get()) };
		auto* rhs{ emit(node.rhs.get()) };

		switch (node.op) {
			case ASTNode::Operator::Add:
				return _builder.emit<Add>(node.type, lhs, rhs);
			case ASTNode::Operator::Sub:
				return _builder.emit<Sub>(node.type, lhs, rhs);
			case ASTNode::Operator::Mul:
				return _builder.emit<Mul>(node.type, lhs, rhs);
			case ASTNode::Operator::Div:
				return _builder.emit<Div>(node.type, lhs, rhs);
			case ASTNode::Operator::Mod:
				return _builder.emit<Mod>(node.type, lhs, rhs);
			case ASTNode::Operator::Equal:
				return _builder.emit<Equal>(lhs, rhs);
			case ASTNode::Operator::NotEqual:
				return _builder.emit<NotEqual>(lhs, rhs);
			case ASTNode::Operator::Greater:
				return _builder.emit<Greater>(lhs, rhs);
			case ASTNode::Operator::GreaterEqual:
				return _builder.emit<GreaterEqual>(lhs, rhs);
			case ASTNode::Operator::Less:
				return _builder.emit<Less>(lhs, rhs);
			case ASTNode::Operator::LessEqual:
				return _builder.emit<LessEqual>(lhs, rhs);
			case ASTNode::Operator::LogicalAnd:
				return _builder.emit<And>(lhs, rhs);
			case ASTNode::Operator::LogicalOr:
				return _builder.emit<Or>(lhs, rhs);
			default:
				break;
		}

		// ERROR: unhandled case OR non-desugared instruction present.
		return _builder.emit<Unreachable>();
	}

	ir::Instruction* LowerVisitor::emit(const BlockNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.emit<Unreachable>();
	}

	ir::Instruction* LowerVisitor::emit(const CastNode& node)
	{
		return _builder.emit<Cast>(node.type, emit(node.expression.get()));
	}

	ir::Instruction* LowerVisitor::emit(const ErrorNode&)
	{
		// ERROR: AST should be in a valid state at this point.
		return _builder.emit<Unreachable>();
	}

	ir::Instruction* LowerVisitor::emit(const ForLoopNode&)
	{
		// ERROR: should've been replaced with WhileNode at this point.
		return _builder.emit<Unreachable>();
	}

	ir::Instruction* LowerVisitor::emit(const ForeachLoopNode&)
	{
		// ERROR: should've been replaced with ForLoop node at this point.
		return _builder.emit<Unreachable>();
	}

	ir::Instruction* LowerVisitor::emit(const FunctionCallNode& node)
	{
		std::vector<Instruction*> parameters;
		parameters.reserve(node.parameters.size());
		for (const auto& parameter : node.parameters) {
			parameters.emplace_back(emit(parameter.get()));
		}
		return _builder.emit<Call>(node.type, node.name, std::move(parameters));
	}

	ir::Instruction* LowerVisitor::emit(const FunctionDeclarationNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.emit<Unreachable>();
	}

	ir::Instruction* LowerVisitor::emit(const IfNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.emit<Unreachable>();
	}

	ir::Instruction* LowerVisitor::emit(const LiteralNode& node)
	{
		// IMPORTANT: We assume that visiting LiteralNode will always result in the READ operation for Identifiers, as
		// any special (i.e. writing) logic will be handled beforehand.
		if (node.value.is<Identifier>()) {
			auto* slot = _builder.get_slot(node.value.as<Identifier>());
			return _builder.emit<StackLoad>(slot);
		}
		return _builder.emit<Const>(node.type, node.value);
	}

	ir::Instruction* LowerVisitor::emit(const ModuleNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.emit<Unreachable>();
	}

	ir::Instruction* LowerVisitor::emit(const ReturnNode& node)
	{
		auto* expression{ emit(node.expression.get()) };
		return _builder.emit<Return>(expression);
	}

	ir::Instruction* LowerVisitor::emit(const StructDeclarationNode&)
	{
		// NOTE: ERROR; at this point this node should not be present.
		return _builder.emit<Unreachable>();
	}

	ir::Instruction* LowerVisitor::emit(const UnaryNode& node)
	{
		auto* expression{ emit(node.expression.get()) };
		switch (node.op) {
			case ASTNode::Operator::LogicalNot:
				return _builder.emit<Not>(expression);
			default:
				break;
		}
		// ERROR: unhandled case OR non-desugared instruction present.
		return _builder.emit<Unreachable>();
	}

	ir::Instruction* LowerVisitor::emit(const VariableDeclarationNode& node)
	{
		auto* slot{ _builder.get_slot(node.name) };
		auto* value{ emit(node.expression.get()) };
		return _builder.emit<StackStore>(slot, value);
	}

	ir::Instruction* LowerVisitor::emit(const WhileNode&)
	{
		// ERROR: should NEVER be visited directly.
		return _builder.emit<Unreachable>();
	}
}  // namespace soul::ast::visitors
