#include "ast/visitors/type_discoverer.h"

#include "common/types/type.h"
#include "core/types.h"

namespace soul::ast::visitors
{
	using namespace soul::types;

	TypeDiscovererVisitor::TypeMap TypeDiscovererVisitor::discovered_types() noexcept { return _registered_types; }

	void TypeDiscovererVisitor::visit(const StructDeclarationNode& node)
	{
		if (_registered_types.contains(node.name)) {
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
			if (!_registered_types.contains(param.type_identifier)) {
				struct_declaration.parameters[index] = ErrorNode::create(
					std::format("cannot resolve type '{}', because no such type exists", param.type_identifier));
				continue;
			}
			contained_types.push_back(_registered_types.at(param.type_identifier));
		}
		_registered_types[node.name] = Type{ StructType{ std::move(contained_types) } };
	}
	TypeDiscovererVisitor::TypeMap TypeDiscovererVisitor::basic_types() noexcept
	{
		// IMPORTANT: Must match keywords defined in Lexer::scan_token.
		using namespace std::string_view_literals;
		static const TypeDiscovererVisitor::TypeMap k_basic_types = {
			{ "bool"sv, types::PrimitiveType::Kind::Boolean },
            { "chr"sv,  types::PrimitiveType::Kind::Char    },
			{ "f32"sv,  types::PrimitiveType::Kind::Float32 },
            { "f64"sv,  types::PrimitiveType::Kind::Float64 },
			{ "i32"sv,  types::PrimitiveType::Kind::Int32   },
            { "i64"sv,  types::PrimitiveType::Kind::Int64   },
			{ "str"sv,  types::PrimitiveType::Kind::String  },
            { "void"sv, types::PrimitiveType::Kind::Void    },
		};
		return k_basic_types;
	}
}  // namespace soul::ast::visitors
