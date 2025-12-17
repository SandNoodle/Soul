#pragma once

#include "AST/AST.h"
#include "AST/ASTFwd.h"
#include "AST/Visitors/DefaultTraverse.h"

#include "IR/Builder.h"
#include "IR/IR.h"
#include "IR/Instruction.h"

#include <vector>

namespace Soul::AST::Visitors
{
	/**
	 * @brief CodegenVisitor lowers the AST into langauge's IR.
	 * It is the last visitor that performs traversal on the AST.
	 */
	class LowerVisitor : public DefaultTraverseVisitor
	{
		private:
		using LoopJumpTargets
			= std::vector<std::pair<IR::BasicBlock* /* continuation */, IR::BasicBlock* /* termination */>>;

		private:
		IR::IRBuilder _builder{};
		IR::Instruction* _current_instruction{ nullptr };
		LoopJumpTargets _loop_jump_targets{};

		public:
		/** @brief Returns IR representation equivalent to the AST. */
		std::unique_ptr<IR::Module> get() noexcept;

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
		IR::Instruction* emit(const ASTNode::Reference);
		IR::Instruction* emit(const BinaryNode&);
		IR::Instruction* emit(const BlockNode&);
		IR::Instruction* emit(const CastNode&);
		IR::Instruction* emit(const ErrorNode&);
		IR::Instruction* emit(const ForLoopNode&);
		IR::Instruction* emit(const ForeachLoopNode&);
		IR::Instruction* emit(const FunctionCallNode&);
		IR::Instruction* emit(const FunctionDeclarationNode&);
		IR::Instruction* emit(const IfNode&);
		IR::Instruction* emit(const LiteralNode&);
		IR::Instruction* emit(const LoopControlNode&);
		IR::Instruction* emit(const ModuleNode&);
		IR::Instruction* emit(const ReturnNode&);
		IR::Instruction* emit(const StructDeclarationNode&);
		IR::Instruction* emit(const UnaryNode&);
		IR::Instruction* emit(const VariableDeclarationNode&);
		IR::Instruction* emit(const WhileNode&);
	};
}  // namespace Soul::AST::Visitors
