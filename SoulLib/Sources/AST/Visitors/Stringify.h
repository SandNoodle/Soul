#pragma once

#include "AST/AST.h"
#include "AST/ASTFwd.h"
#include "AST/Visitors/DefaultTraverse.h"
#include "Core/Types.h"

#include <sstream>

namespace Soul::AST::Visitors
{
	/**
	 * @brief StringifyVisitor traverses the AST and converts each visited node into its (valid) JSON representation.
	 * It is useful for debugging.
	 */
	class StringifyVisitor final : public DefaultTraverseVisitor
	{
		public:
		enum Options : UInt8
		{
			NONE        = 0 << 0,
			PRINT_TYPES = 1 << 0,
		};

		private:
		static constexpr auto k_unnamed              = "__unnamed__";
		static constexpr std::size_t k_indent_amount = 2;

		private:
		std::stringstream _ss;
		std::size_t _indent_level = 0;
		Options _options{};

		public:
		StringifyVisitor(Options options = NONE);

		/** @brief Returns textual representation of an AST. */
		std::string string() const;

		void Accept(const ASTNode::Reference node) override;

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

		private:
		std::string CurrentIndent() const;
		void Encode(std::string_view key, std::string_view value, bool add_trailing_comma = true);
		void EncodeType(const Types::Type& type);
		void Encode(std::string_view key, const ASTNode::Reference node, bool add_trailing_comma = true);
		template <std::ranges::forward_range T>
		void Encode(std::string_view key, const T& parameters, bool add_trailing_comma = true)
			requires(std::same_as<ASTNode::Dependency, std::ranges::range_value_t<T>>);
	};
}  // namespace Soul::AST::Visitors
#include "AST/Visitors/Stringify.inl"
