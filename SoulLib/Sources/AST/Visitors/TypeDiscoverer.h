#pragma once

#include "AST/AST.h"
#include "AST/ASTFwd.h"
#include "AST/Visitors/Copy.h"
#include "Parser/TypeSpecifier.h"
#include "Types/TypeFwd.h"

#include <tuple>
#include <vector>

namespace Soul::AST::Visitors
{
	/**
	 * @brief TypeDiscovererVisitor traverses the AST while making note of each type declaration.
	 */
	class TypeDiscovererVisitor final : public CopyVisitor
	{
		public:
		using TypeDeclarations = std::vector<std::pair<Parser::TypeSpecifier, Types::Type>>;

		private:
		TypeDeclarations _registered_types = GetBasicTypes();

		public:
		TypeDiscovererVisitor()                                 = default;
		TypeDiscovererVisitor(const TypeDiscovererVisitor&)     = delete;
		TypeDiscovererVisitor(TypeDiscovererVisitor&&) noexcept = default;
		~TypeDiscovererVisitor() override                       = default;

		TypeDiscovererVisitor& operator=(const TypeDiscovererVisitor&)     = delete;
		TypeDiscovererVisitor& operator=(TypeDiscovererVisitor&&) noexcept = default;

		/** @brief Returns name-to-type map of all discovered (and basic) types for a given AST. */
		TypeDeclarations GetDiscoveredTypes() noexcept;

		static TypeDeclarations GetBasicTypes() noexcept;

		using CopyVisitor::Accept;

		protected:
		using CopyVisitor::Visit;
		void Visit(const StructDeclarationNode&) override;
	};
}  // namespace Soul::AST::Visitors
