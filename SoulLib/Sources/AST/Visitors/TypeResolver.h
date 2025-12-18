#pragma once

#include "AST/AST.h"
#include "AST/ASTFwd.h"
#include "AST/Visitors/Copy.h"
#include "AST/Visitors/TypeDiscoverer.h"
#include "Types/Type.h"

#include <optional>
#include <ranges>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

namespace Soul::AST::Visitors
{
	/**
	 * @brief TypeResolverVisitor traverses the AST while resolving each node into the correct type.
	 */
	class TypeResolverVisitor final : public CopyVisitor
	{
		public:
		using TypeDeclarations = TypeDiscovererVisitor::TypeDeclarations;

		private:
		struct FunctionDeclaration
		{
			std::vector<Types::Type> input_types;
			Types::Type return_type;
		};

		using VariableContext = std::vector<std::pair<std::string, Types::Type>>;
		using FunctionContext = std::vector<std::pair<std::string, FunctionDeclaration>>;

		private:
		TypeDeclarations _registered_types;
		VariableContext _variables_in_scope;
		FunctionContext _functions_in_module;

		public:
		TypeResolverVisitor(TypeDeclarations type_map);
		TypeResolverVisitor(const TypeResolverVisitor&)     = delete;
		TypeResolverVisitor(TypeResolverVisitor&&) noexcept = default;
		~TypeResolverVisitor()                              = default;

		TypeResolverVisitor& operator=(const TypeResolverVisitor&)     = delete;
		TypeResolverVisitor& operator=(TypeResolverVisitor&&) noexcept = default;

		using CopyVisitor::Accept;

		protected:
		using CopyVisitor::Visit;
		void Visit(const BinaryNode&) override;
		void Visit(const BlockNode&) override;
		void Visit(const CastNode&) override;
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
		Types::Type GetTypeOrDefault(const Parser::TypeSpecifier& type_specifier) const noexcept;
		std::optional<Types::Type> GetVariableType(std::string_view name) const noexcept;
		Types::Type GetTypeForOperator(ASTNode::Operator op,
		                               const std::ranges::forward_range auto& input_types) const noexcept;
		std::optional<FunctionDeclaration> GetFunctionDeclaration(
			std::string_view name,
			const std::ranges::forward_range auto& want_types) const noexcept;
	};
}  // namespace Soul::AST::Visitors
#include "AST/Visitors/TypeResolver.inl"
