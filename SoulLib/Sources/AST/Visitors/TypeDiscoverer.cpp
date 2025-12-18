#include "AST/Visitors/TypeDiscoverer.h"

#include "Core/Types.h"
#include "Types/Type.h"

namespace Soul::AST::Visitors
{
	using namespace Soul::Types;
	using namespace Soul::Parser;

	TypeDiscovererVisitor::TypeDeclarations TypeDiscovererVisitor::GetDiscoveredTypes() noexcept
	{
		return _registered_types;
	}

	void TypeDiscovererVisitor::Visit(const StructDeclarationNode& node)
	{
		BaseTypeSpecifier type_specifier = BaseTypeSpecifier{ node.name };
		const auto type_it{ std::ranges::find(
			_registered_types, type_specifier, &decltype(_registered_types)::value_type::first) };
		if (type_it != std::end(_registered_types)) {
			_current_clone = ErrorNode::Create(std::format("redefinition of type '{}'", node.name));
			return;
		}

		CopyVisitor::Visit(node);
		auto& struct_declaration = _current_clone->As<StructDeclarationNode>();

		StructType::ContainedTypes contained_types{};
		contained_types.reserve(node.parameters.size());
		for (std::size_t index = 0; index < node.parameters.size(); ++index) {
			if (!node.parameters[index]->Is<VariableDeclarationNode>()) {
				struct_declaration.parameters[index] = ErrorNode::Create(std::format(
					"[INTERNAL] cannot resolve type for '{}', because parameter is not of valid (node) type",
					node.name));
				continue;
			}
			const auto& param = node.parameters[index]->As<VariableDeclarationNode>();
			const auto it{ std::ranges::find(
				_registered_types, param.type_specifier, &decltype(_registered_types)::value_type::first) };
			if (it == std::end(_registered_types)) {
				struct_declaration.parameters[index] = ErrorNode::Create(std::format(
					"cannot resolve type '{}', because no such type exists", std::string(param.type_specifier)));
				continue;
			}
			contained_types.push_back(it->second);
		}
		_registered_types.emplace_back(
			std::make_pair(type_specifier, Type{ StructType{ std::move(contained_types) } }));
	}
	TypeDiscovererVisitor::TypeDeclarations TypeDiscovererVisitor::GetBasicTypes() noexcept
	{
		using namespace std::string_view_literals;
		static const TypeDeclarations k_basic_types = {
			std::make_pair(k_base_specifier_i8, PrimitiveType::Kind::INT8),
			std::make_pair(k_base_specifier_i16, PrimitiveType::Kind::INT16),
			std::make_pair(k_base_specifier_i32, PrimitiveType::Kind::INT32),
			std::make_pair(k_base_specifier_i64, PrimitiveType::Kind::INT64),
			std::make_pair(k_base_specifier_i128, PrimitiveType::Kind::INT128),

			std::make_pair(k_base_specifier_u8, PrimitiveType::Kind::UINT8),
			std::make_pair(k_base_specifier_u16, PrimitiveType::Kind::UINT16),
			std::make_pair(k_base_specifier_u32, PrimitiveType::Kind::UINT32),
			std::make_pair(k_base_specifier_u64, PrimitiveType::Kind::UINT64),
			std::make_pair(k_base_specifier_u128, PrimitiveType::Kind::UINT128),

			std::make_pair(k_base_specifier_f32, PrimitiveType::Kind::FLOAT32),
			std::make_pair(k_base_specifier_f64, PrimitiveType::Kind::FLOAT64),

			std::make_pair(k_base_specifier_chr, PrimitiveType::Kind::CHAR),
			std::make_pair(k_base_specifier_str, PrimitiveType::Kind::STRING),

			std::make_pair(k_base_specifier_bool, PrimitiveType::Kind::BOOLEAN),
			std::make_pair(k_base_specifier_isize, PrimitiveType::Kind::ISIZE),
			std::make_pair(k_base_specifier_usize, PrimitiveType::Kind::USIZE),

			std::make_pair(k_base_specifier_void, PrimitiveType::Kind::VOID),
		};
		return k_basic_types;
	}
}  // namespace Soul::AST::Visitors
