#pragma once

#include "ast/ast.h"
#include "ast/ast_fwd.h"
#include "ast/visitors/copy.h"
#include "common/types/types_fwd.h"

#include <string_view>
#include <unordered_map>

namespace soul::ast::visitors
{
	/**
	 * @brief TypeDiscovererVisitor traverses the AST while making note of each type declaration.
	 */
	class TypeDiscovererVisitor final : public CopyVisitor
	{
		public:
		using TypeMap = std::unordered_map<std::string_view, types::Type>;

		private:
		TypeMap _registered_types = basic_types();

		public:
		TypeDiscovererVisitor()                                 = default;
		TypeDiscovererVisitor(const TypeDiscovererVisitor&)     = delete;
		TypeDiscovererVisitor(TypeDiscovererVisitor&&) noexcept = default;
		~TypeDiscovererVisitor()                                = default;

		TypeDiscovererVisitor& operator=(const TypeDiscovererVisitor&)     = delete;
		TypeDiscovererVisitor& operator=(TypeDiscovererVisitor&&) noexcept = default;

		/** @brief Returns name-to-type map of all discovered (and basic) types for a given AST. */
		TypeMap discovered_types() noexcept;

		static TypeMap basic_types() noexcept;

		using CopyVisitor::accept;

		protected:
		using CopyVisitor::visit;
		void visit(const StructDeclarationNode&) override;
	};
}  // namespace soul::ast::visitors
