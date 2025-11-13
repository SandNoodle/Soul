#pragma once

#include "ast/ast_fwd.h"
#include "ast/visitors/copy.h"

namespace soul::ast::visitors
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
		using CopyVisitor::accept;

		protected:
		using CopyVisitor::visit;
		void visit(const BinaryNode&) override;
		void visit(const BlockNode&) override;
		void visit(const ForLoopNode&) override;
		void visit(const ForeachLoopNode&) override;
		void visit(const FunctionDeclarationNode&) override;
		void visit(const LiteralNode&) override;
		void visit(const LoopControlNode&) override;
		void visit(const VariableDeclarationNode&) override;
		void visit(const WhileNode&) override;

		private:
		VariableDeclarationNode* get_variable(std::string_view identifier);
	};
}  // namespace soul::ast::visitors
