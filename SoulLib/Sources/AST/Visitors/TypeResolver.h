#pragma once

#include "AST/AST.h"
#include "AST/ASTFwd.h"
#include "AST/Visitors/Copy.h"
#include "AST/Visitors/TypeDiscoverer.h"
#include "Types/TypeFwd.h"

#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <tuple>
#include <vector>

namespace soul::ast::visitors
{
	/**
	 * @brief TypeResolverVisitor traverses the AST while resolving each node into the correct type.
	 */
	class TypeResolverVisitor final : public CopyVisitor
	{
		public:
		using Types = TypeDiscovererVisitor::Types;

		private:
		struct FunctionDeclaration
		{
			std::vector<types::Type> input_types;
			types::Type return_type;
		};

		using VariableContext = std::vector<std::pair<std::string, types::Type>>;
		using FunctionContext = std::vector<std::pair<std::string, FunctionDeclaration>>;

		private:
		Types _registered_types;
		VariableContext _variables_in_scope;
		FunctionContext _functions_in_module;

		public:
		TypeResolverVisitor(Types type_map);
		TypeResolverVisitor(const TypeResolverVisitor&)     = delete;
		TypeResolverVisitor(TypeResolverVisitor&&) noexcept = default;
		~TypeResolverVisitor()                              = default;

		TypeResolverVisitor& operator=(const TypeResolverVisitor&)     = delete;
		TypeResolverVisitor& operator=(TypeResolverVisitor&&) noexcept = default;

		using CopyVisitor::accept;

		protected:
		using CopyVisitor::visit;
		void visit(const BinaryNode&) override;
		void visit(const BlockNode&) override;
		void visit(const CastNode&) override;
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
		types::Type get_type_or_default(const parser::TypeSpecifier& type_specifier) const noexcept;
		std::optional<types::Type> get_variable_type(std::string_view name) const noexcept;
		types::Type get_type_for_operator(ASTNode::Operator op,
		                                  const std::ranges::forward_range auto& input_types) const noexcept;
		std::optional<FunctionDeclaration> get_function_declaration(
			std::string_view name,
			const std::ranges::forward_range auto& want_types) const noexcept;
	};
}  // namespace soul::ast::visitors
#include "AST/Visitors/TypeResolver.inl"
