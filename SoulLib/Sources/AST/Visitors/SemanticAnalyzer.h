#pragma once

#include "AST/ASTFwd.h"
#include "AST/Visitors/Copy.h"

namespace Soul::AST::Visitors
{
	/**
	 * @brief SemanticAnalyzerVisitor traverses the AST while verifying that each construct present is valid and
	 *  appears in the allowed places.
	 */
	class SemanticAnalyzerVisitor : public CopyVisitor
	{
		private:
		using VariableContext = std::vector<VariableDeclarationNode*>;

		private:
		bool _is_in_loop{};
		std::size_t _current_depth{};
		VariableContext _variables_in_scope{};

		public:
		using CopyVisitor::Accept;

		protected:
		using CopyVisitor::Visit;
		void Visit(const BinaryNode&) override;
		void Visit(const BlockNode&) override;
		void Visit(const ForLoopNode&) override;
		void Visit(const ForeachLoopNode&) override;
		void Visit(const FunctionDeclarationNode&) override;
		void Visit(const LiteralNode&) override;
		void Visit(const LoopControlNode&) override;
		void Visit(const VariableDeclarationNode&) override;
		void Visit(const WhileNode&) override;

		private:
		VariableDeclarationNode* GetVariable(std::string_view identifier);
	};
}  // namespace Soul::AST::Visitors
