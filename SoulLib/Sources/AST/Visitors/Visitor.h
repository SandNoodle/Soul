#pragma once

#include "AST/ASTFwd.h"

namespace Soul::AST::Visitors
{
	/**
	 * @brief Base class for visiting nodes in the Abstract Syntax Tree.
	 */
	class IVisitor
	{
		public:
		virtual ~IVisitor() = default;

		/**
		 * @brief Returns true if a visitor modifies or affects the AST in any shape or form, i.e. does not perform
		 * `read-only` operations.
		 */
		[[nodiscard]] virtual constexpr bool affects() const noexcept { return false; }

		virtual constexpr void visit(const BinaryNode&) {}
		virtual constexpr void visit(const BlockNode&) {}
		virtual constexpr void visit(const CastNode&) {}
		virtual constexpr void visit(const ErrorNode&) {}
		virtual constexpr void visit(const ForLoopNode&) {}
		virtual constexpr void visit(const ForeachLoopNode&) {}
		virtual constexpr void visit(const FunctionCallNode&) {}
		virtual constexpr void visit(const FunctionDeclarationNode&) {}
		virtual constexpr void visit(const IfNode&) {}
		virtual constexpr void visit(const LiteralNode&) {}
		virtual constexpr void visit(const LoopControlNode&) {}
		virtual constexpr void visit(const ModuleNode&) {}
		virtual constexpr void visit(const ReturnNode&) {}
		virtual constexpr void visit(const StructDeclarationNode&) {}
		virtual constexpr void visit(const UnaryNode&) {}
		virtual constexpr void visit(const VariableDeclarationNode&) {}
		virtual constexpr void visit(const WhileNode&) {}
	};
}  // namespace Soul::AST::Visitors
