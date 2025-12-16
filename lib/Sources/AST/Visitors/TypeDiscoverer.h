#pragma once

#include "AST/Visitors/copy.h"
#include "AST/ast.h"
#include "AST/ast_fwd.h"
#include "Parser/TypeSpecifier.h"
#include "Types/types_fwd.h"

#include <string_view>
#include <tuple>
#include <vector>

namespace Soul::ast::visitors
{
	/**
	 * @brief TypeDiscovererVisitor traverses the AST while making note of each type declaration.
	 */
	class TypeDiscovererVisitor final : public CopyVisitor
	{
		public:
		using Types = std::vector<std::pair<parser::TypeSpecifier, types::Type>>;

		private:
		Types _registered_types = basic_types();

		public:
		TypeDiscovererVisitor()                                 = default;
		TypeDiscovererVisitor(const TypeDiscovererVisitor&)     = delete;
		TypeDiscovererVisitor(TypeDiscovererVisitor&&) noexcept = default;
		~TypeDiscovererVisitor() override                       = default;

		TypeDiscovererVisitor& operator=(const TypeDiscovererVisitor&)     = delete;
		TypeDiscovererVisitor& operator=(TypeDiscovererVisitor&&) noexcept = default;

		/** @brief Returns name-to-type map of all discovered (and basic) types for a given AST. */
		Types discovered_types() noexcept;

		static Types basic_types() noexcept;

		using CopyVisitor::accept;

		protected:
		using CopyVisitor::visit;
		void visit(const StructDeclarationNode&) override;
	};
}  // namespace Soul::ast::visitors
