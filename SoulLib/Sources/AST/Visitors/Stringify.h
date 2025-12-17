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
	 * Its useful for debugging.
	 */
	class StringifyVisitor final : public DefaultTraverseVisitor
	{
		public:
		enum Options : u8
		{
			None       = 0 << 0,
			PrintTypes = 1 << 0,
		};

		private:
		static constexpr auto k_unnamed              = "__unnamed__";
		static constexpr std::size_t k_indent_amount = 2;

		private:
		std::stringstream _ss;
		std::size_t _indent_level = 0;
		Options _options{};

		public:
		StringifyVisitor(Options options = Options::None);

		/** @brief Returns textual representation of an AST. */
		std::string string() const;

		void accept(const ASTNode::Reference node) override;

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

		private:
		std::string current_indent() const;
		void encode(std::string_view key, std::string_view value, bool add_trailing_comma = true);
		void encode_type(const Types::Type& type);
		void encode(std::string_view key, const ASTNode::Reference node, bool add_trailing_comma = true);
		template <std::ranges::forward_range T>
		void encode(std::string_view key, const T& parameters, bool add_trailing_comma = true)
			requires(std::same_as<ASTNode::Dependency, std::ranges::range_value_t<T>>);
	};
}  // namespace Soul::AST::Visitors
#include "AST/Visitors/Stringify.inl"
