#pragma once

#include "ast/ast.h"
#include "ast/ast_fwd.h"
#include "ast/visitors/default_traverse.h"

#include "ir/builder.h"
#include "ir/instruction.h"
#include "ir/ir.h"

#include <vector>

namespace soul::ast::visitors
{
	/**
	 * @brief CodegenVisitor lowers the AST into langauge's IR.
	 * It is the last visitor that performs traversal on the AST.
	 */
	class LowerVisitor : public DefaultTraverseVisitor
	{
		private:
		using LoopJumpTargets
			= std::vector<std::pair<ir::BasicBlock* /* continuation */, ir::BasicBlock* /* termination */>>;

		private:
		ir::IRBuilder _builder{};
		ir::Instruction* _current_instruction{ nullptr };
		LoopJumpTargets _loop_jump_targets{};

		public:
		/** @brief Returns IR representation equivalent to the AST. */
		std::unique_ptr<ir::Module> get() noexcept;

		using DefaultTraverseVisitor::accept;

		protected:
		using DefaultTraverseVisitor::visit;
		void visit(const BinaryNode&) override;
		void visit(const BlockNode&) override;
		void visit(const CastNode&) override;
		void visit(const ErrorNode&) override;
		void visit(const ForLoopNode&) override;
		void visit(const ForeachLoopNode&) override;
		void visit(const FunctionCallNode&) override;
		void visit(const FunctionDeclarationNode&) override;
		void visit(const IfNode&) override;
		void visit(const LiteralNode&) override;
		void visit(const LoopControlNode&) override;
		void visit(const ModuleNode&) override;
		void visit(const ReturnNode&) override;
		void visit(const StructDeclarationNode&) override;
		void visit(const UnaryNode&) override;
		void visit(const VariableDeclarationNode&) override;
		void visit(const WhileNode&) override;

		/**
		 * @brief Visits the specified node and emits its equivalent instruction(s).
		 * @warning Some nodes may not be visited directly and should be handled separately (see implementation).
		 */
		ir::Instruction* emit(const ASTNode::Reference);
		ir::Instruction* emit(const BinaryNode&);
		ir::Instruction* emit(const BlockNode&);
		ir::Instruction* emit(const CastNode&);
		ir::Instruction* emit(const ErrorNode&);
		ir::Instruction* emit(const ForLoopNode&);
		ir::Instruction* emit(const ForeachLoopNode&);
		ir::Instruction* emit(const FunctionCallNode&);
		ir::Instruction* emit(const FunctionDeclarationNode&);
		ir::Instruction* emit(const IfNode&);
		ir::Instruction* emit(const LiteralNode&);
		ir::Instruction* emit(const LoopControlNode&);
		ir::Instruction* emit(const ModuleNode&);
		ir::Instruction* emit(const ReturnNode&);
		ir::Instruction* emit(const StructDeclarationNode&);
		ir::Instruction* emit(const UnaryNode&);
		ir::Instruction* emit(const VariableDeclarationNode&);
		ir::Instruction* emit(const WhileNode&);
	};
}  // namespace soul::ast::visitors
