#include "ast/visitors/type_discoverer.h"

#include "core/types.h"
#include "types/type.h"

namespace soul::ast::visitors
{
	using namespace soul::types;
	using namespace soul::parser;

	TypeDiscovererVisitor::Types TypeDiscovererVisitor::discovered_types() noexcept { return _registered_types; }

	void TypeDiscovererVisitor::visit(const StructDeclarationNode& node)
	{
		BaseTypeSpecifier type_specifier = BaseTypeSpecifier{ node.name };
		const auto type_it{ std::ranges::find(
			_registered_types, type_specifier, &decltype(_registered_types)::value_type::first) };
		if (type_it != std::end(_registered_types)) {
			_current_clone = ErrorNode::create(std::format("redefinition of type '{}'", node.name));
			return;
		}

		CopyVisitor::visit(node);
		auto& struct_declaration = _current_clone->as<StructDeclarationNode>();

		StructType::ContainedTypes contained_types{};
		contained_types.reserve(node.parameters.size());
		for (std::size_t index = 0; index < node.parameters.size(); ++index) {
			if (!node.parameters[index]->is<VariableDeclarationNode>()) {
				struct_declaration.parameters[index] = ErrorNode::create(std::format(
					"[INTERNAL] cannot resolve type for '{}', because parameter is not of valid (node) type",
					node.name));
				continue;
			}
			const auto& param = node.parameters[index]->as<VariableDeclarationNode>();
			const auto it{ std::ranges::find(
				_registered_types, param.type_specifier, &decltype(_registered_types)::value_type::first) };
			if (it == std::end(_registered_types)) {
				struct_declaration.parameters[index] = ErrorNode::create(std::format(
					"cannot resolve type '{}', because no such type exists", std::string(param.type_specifier)));
				continue;
			}
			contained_types.push_back(it->second);
		}
		_registered_types.emplace_back(
			std::make_pair(type_specifier, Type{ StructType{ std::move(contained_types) } }));
	}
	TypeDiscovererVisitor::Types TypeDiscovererVisitor::basic_types() noexcept
	{
		using namespace std::string_view_literals;
		static const TypeDiscovererVisitor::Types k_basic_types = {
			std::make_pair(k_base_specifier_bool, types::PrimitiveType::Kind::Boolean),
			std::make_pair(k_base_specifier_chr, types::PrimitiveType::Kind::Char),
			std::make_pair(k_base_specifier_f32, types::PrimitiveType::Kind::Float32),
			std::make_pair(k_base_specifier_f64, types::PrimitiveType::Kind::Float64),
			std::make_pair(k_base_specifier_i32, types::PrimitiveType::Kind::Int32),
			std::make_pair(k_base_specifier_i64, types::PrimitiveType::Kind::Int64),
			std::make_pair(k_base_specifier_str, types::PrimitiveType::Kind::String),
			std::make_pair(k_base_specifier_void, types::PrimitiveType::Kind::Void),
		};
		return k_basic_types;
	}
}  // namespace soul::ast::visitors
