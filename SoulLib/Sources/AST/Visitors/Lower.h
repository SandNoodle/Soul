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
	 * @brief CodegenVisitor lowers the AST into language's IR.
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
		std::unique_ptr<IR::Module> Get() noexcept;

		using DefaultTraverseVisitor::Accept;

		protected:
		using DefaultTraverseVisitor::Visit;
		void Visit(const BinaryNode&) override;
		void Visit(const BlockNode&) override;
		void Visit(const CastNode&) override;
		void Visit(const ErrorNode&) override;
		void Visit(const ForLoopNode&) override;
		void Visit(const ForeachLoopNode&) override;
		void Visit(const FunctionCallNode&) override;
		void Visit(const FunctionDeclarationNode&) override;
		void Visit(const IfNode&) override;
		void Visit(const LiteralNode&) override;
		void Visit(const LoopControlNode&) override;
		void Visit(const ModuleNode&) override;
		void Visit(const ReturnNode&) override;
		void Visit(const StructDeclarationNode&) override;
		void Visit(const UnaryNode&) override;
		void Visit(const VariableDeclarationNode&) override;
		void Visit(const WhileNode&) override;

		/**
		 * @brief Visits the specified node and emits its equivalent instruction(s).
		 * @warning Some nodes may not be visited directly and should be handled separately (see implementation).
		 */
		IR::Instruction* Emit(const ASTNode::Reference);
		IR::Instruction* Emit(const BinaryNode&);
		IR::Instruction* Emit(const BlockNode&);
		IR::Instruction* Emit(const CastNode&);
		IR::Instruction* Emit(const ErrorNode&);
		IR::Instruction* Emit(const ForLoopNode&);
		IR::Instruction* Emit(const ForeachLoopNode&);
		IR::Instruction* Emit(const FunctionCallNode&);
		IR::Instruction* Emit(const FunctionDeclarationNode&);
		IR::Instruction* Emit(const IfNode&);
		IR::Instruction* Emit(const LiteralNode&);
		IR::Instruction* Emit(const LoopControlNode&);
		IR::Instruction* Emit(const ModuleNode&);
		IR::Instruction* Emit(const ReturnNode&);
		IR::Instruction* Emit(const StructDeclarationNode&);
		IR::Instruction* Emit(const UnaryNode&);
		IR::Instruction* Emit(const VariableDeclarationNode&);
		IR::Instruction* Emit(const WhileNode&);
	};
}  // namespace Soul::AST::Visitors
