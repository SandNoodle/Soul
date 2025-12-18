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
		[[nodiscard]] virtual constexpr bool Affects() const noexcept { return false; }

		virtual constexpr void Visit(const BinaryNode&) {}
		virtual constexpr void Visit(const BlockNode&) {}
		virtual constexpr void Visit(const CastNode&) {}
		virtual constexpr void Visit(const ErrorNode&) {}
		virtual constexpr void Visit(const ForLoopNode&) {}
		virtual constexpr void Visit(const ForeachLoopNode&) {}
		virtual constexpr void Visit(const FunctionCallNode&) {}
		virtual constexpr void Visit(const FunctionDeclarationNode&) {}
		virtual constexpr void Visit(const IfNode&) {}
		virtual constexpr void Visit(const LiteralNode&) {}
		virtual constexpr void Visit(const LoopControlNode&) {}
		virtual constexpr void Visit(const ModuleNode&) {}
		virtual constexpr void Visit(const ReturnNode&) {}
		virtual constexpr void Visit(const StructDeclarationNode&) {}
		virtual constexpr void Visit(const UnaryNode&) {}
		virtual constexpr void Visit(const VariableDeclarationNode&) {}
		virtual constexpr void Visit(const WhileNode&) {}
	};
}  // namespace Soul::AST::Visitors
