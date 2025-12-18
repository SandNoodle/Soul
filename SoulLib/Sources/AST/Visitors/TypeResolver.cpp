#include "AST/Visitors/TypeResolver.h"

#include "Core/Types.h"
#include "Types/Type.h"

#include <array>
#include <format>

namespace Soul::AST::Visitors
{
	using namespace Soul::AST;
	using namespace Soul::Parser;
	using namespace Soul::Types;

	CastNode::Type GetCastType(const Type& from_type, const Type& to_type);

	TypeResolverVisitor::TypeResolverVisitor(TypeDeclarations type_map) : _registered_types(std::move(type_map)) {}

	void TypeResolverVisitor::Visit(const BinaryNode& node)
	{
		CopyVisitor::Visit(node);
		auto& binary_node = _current_clone->As<BinaryNode>();

		if (!node.lhs) {
			binary_node.lhs = ErrorNode::Create("[INTERNAL] BinaryNode does not contain LHS expression (nullptr)");
		}
		if (!node.rhs) {
			binary_node.rhs = ErrorNode::Create("[INTERNAL] BinaryNode does not contain RHS expression (nullptr)");
		}
		if (binary_node.lhs->Is<ErrorNode>() || binary_node.rhs->Is<ErrorNode>()) {
			return;
		}

		const auto result_type
			= GetTypeForOperator(binary_node.op, std::array{ binary_node.lhs->type, binary_node.rhs->type });
		if (result_type == Type{}) {
			_current_clone = ErrorNode::Create(std::format("operator ('{}') does not exist for types '{}' and '{}'",
			                                               ASTNode::Name(binary_node.op),
			                                               std::string(binary_node.lhs->type),
			                                               std::string(binary_node.rhs->type)));
			return;
		}
		_current_clone->type = result_type;
	}

	void TypeResolverVisitor::Visit(const BlockNode& node)
	{
		// ENTER SCOPE: Mark a restorepoint for variables declared in the current scope.
		const std::size_t variables_until_this_point = _variables_in_scope.size();

		CopyVisitor::Visit(node);
		_current_clone->type = PrimitiveType::Kind::VOID;

		// EXIT SCOPE: Remove variables defined in that scope.
		const std::size_t variables_declared_in_scope = _variables_in_scope.size() - variables_until_this_point;
		_variables_in_scope.erase(_variables_in_scope.end() - static_cast<std::ptrdiff_t>(variables_declared_in_scope),
		                          _variables_in_scope.end());
	}

	void TypeResolverVisitor::Visit(const CastNode& node)
	{
		CopyVisitor::Visit(node);

		if (!node.expression) {
			_current_clone = ErrorNode::Create("[INTERNAL] CastNode does not contain an expression (nullptr)");
		}

		const auto& cast_node = _current_clone->As<CastNode>();
		const auto from_type  = cast_node.expression->type;
		const auto to_type    = GetTypeOrDefault(cast_node.type_specifier);
		if (GetCastType(from_type, to_type) == CastNode::Type::Impossible) {
			_current_clone = ErrorNode::Create(
				std::format("cannot cast from type '{}' to '{}'", std::string(from_type), std::string(to_type)));
			return;
		}

		_current_clone->type = to_type;
	}

	void TypeResolverVisitor::Visit(const ForLoopNode& node)
	{
		CopyVisitor::Visit(node);
		const auto& for_loop = _current_clone->As<ForLoopNode>();

		if (for_loop.condition) {
			const bool is_condition_bool_coercible
				= GetCastType(for_loop.condition->type, PrimitiveType::Kind::BOOLEAN) != CastNode::Type::Impossible;
			if (!is_condition_bool_coercible) {
				_current_clone = ErrorNode::Create(
					std::format("condition in for loop statement must be convertible to a '{}' type",
				                std::string(Type{ PrimitiveType::Kind::BOOLEAN })));
				return;
			}
		}

		_current_clone->type = PrimitiveType::Kind::VOID;
	}

	void TypeResolverVisitor::Visit(const ForeachLoopNode& node)
	{
		CopyVisitor::Visit(node);
		auto& foreach_node = _current_clone->As<ForeachLoopNode>();

		if (!node.variable) {
			foreach_node.variable
				= ErrorNode::Create("[INTERNAL] ForeachLoopNode does not contain variable expression (nullptr)");
		}
		if (!node.in_expression) {
			foreach_node.in_expression
				= ErrorNode::Create("[INTERNAL] ForeachLoopNode does not contain in_expression expression (nullptr)");
		}
		if (foreach_node.variable->Is<ErrorNode>() || foreach_node.in_expression->Is<ErrorNode>()) {
			return;
		}

		if (!foreach_node.in_expression->type.Is<ArrayType>()) {
			_current_clone = ErrorNode::Create(
				std::format("expression iterated in for each loop statement must be of an array type"));
			return;
		}

		const auto relation_type
			= GetCastType(foreach_node.in_expression->type.As<ArrayType>().DataType(), foreach_node.variable->type);
		if (relation_type == CastNode::Type::Impossible) {
			_current_clone = ErrorNode::Create(std::format(
				"type missmatch in for each loop statement between variable ('{}') and iterated expression ('{}')",
				std::string(foreach_node.variable->type),
				std::string(foreach_node.in_expression->type)));
			return;
		}

		_current_clone->type = PrimitiveType::Kind::VOID;
	}

	void TypeResolverVisitor::Visit(const FunctionCallNode& node)
	{
		CopyVisitor::Visit(node);

		const auto& function_call = _current_clone->As<FunctionCallNode>();
		auto want_types           = function_call.parameters
		                | std::views::transform([](const auto& parameter) -> Type { return parameter->type; });
		const auto function_declaration = GetFunctionDeclaration(node.name, want_types);
		if (!function_declaration.has_value()) {
			_current_clone = ErrorNode::Create(std::format("cannot call non-existing function '{}'", node.name));
			return;
		}
		_current_clone->type = function_declaration->return_type;
	}

	void TypeResolverVisitor::Visit(const FunctionDeclarationNode& node)
	{
		// NOTE: Soul does not support global variables; we can assume that a function declaration is an entirely new
		// scope without any previous declarations.
		_variables_in_scope.clear();

		CopyVisitor::Visit(node);

		auto& function_declaration = _current_clone->As<FunctionDeclarationNode>();
		auto want_types            = function_declaration.parameters
		                | std::views::transform([](const auto& parameter) -> Type { return parameter->type; });
		if (GetFunctionDeclaration(node.name, want_types)) {
			_current_clone
				= ErrorNode::Create(std::format("function declaration '{}' shadows previous one", node.name));
			return;
		}

		for (std::size_t index = 0; index < function_declaration.parameters.size(); ++index) {
			const auto* parameter = function_declaration.parameters[index].get();
			if (parameter->Is<ErrorNode>()) {
				return;
			}
			if (!parameter->Is<VariableDeclarationNode>()) {
				function_declaration.parameters[index]
					= ErrorNode::Create(std::format("[INTERNAL] FunctionDeclarationNode contains "
				                                    "non-VariableDeclarationNode in the parameter list (at {})",
				                                    index));
				return;
			}
		}

		_current_clone->type = GetTypeOrDefault(node.type_specifier);
		_functions_in_module.emplace_back(node.name,
		                                  FunctionDeclaration{
											  .input_types = std::vector<Type>{ want_types.begin(), want_types.end() },
											  .return_type = function_declaration.type
        });
	}

	void TypeResolverVisitor::Visit(const IfNode& node)
	{
		CopyVisitor::Visit(node);

		const auto& if_node = _current_clone->As<IfNode>();
		const bool is_condition_bool_coercible
			= GetCastType(if_node.condition->type, PrimitiveType::Kind::BOOLEAN) != CastNode::Type::Impossible;
		if (!is_condition_bool_coercible) {
			_current_clone = ErrorNode::Create(
				std::format("condition in if statement statement must be convertible to a '{}' type",
			                std::string(Type{ PrimitiveType::Kind::BOOLEAN })));
			return;
		}

		_current_clone->type = PrimitiveType::Kind::VOID;
	}

	void TypeResolverVisitor::Visit(const LiteralNode& node)
	{
		CopyVisitor::Visit(node);

		if (node.value.Is<Identifier>()) {
			const auto& type_identifier = GetVariableType(node.value.As<Identifier>());
			if (!type_identifier) {
				_current_clone = ErrorNode::Create(
					std::format("use of undeclared identifier '{}'", std::string(node.value.As<Identifier>())));
				return;
			}
			_current_clone->type = *type_identifier;
			return;
		}

		_current_clone->type = node.value.GetType();
	}

	void TypeResolverVisitor::Visit(const LoopControlNode& node)
	{
		CopyVisitor::Visit(node);
		_current_clone->type = PrimitiveType::Kind::VOID;
	}

	void TypeResolverVisitor::Visit(const ModuleNode& node)
	{
		CopyVisitor::Visit(node);

		// NOTE: Modules are a collection of type declarations and functions, thus don't have their own type.
		_current_clone->type = PrimitiveType::Kind::VOID;
	}

	void TypeResolverVisitor::Visit(const ReturnNode& node)
	{
		CopyVisitor::Visit(node);
		const auto& expression = _current_clone->As<ReturnNode>().expression;
		_current_clone->type   = expression ? expression->type : PrimitiveType::Kind::VOID;
	}

	void TypeResolverVisitor::Visit(const StructDeclarationNode& node)
	{
		CopyVisitor::Visit(node);
		_current_clone->type = GetTypeOrDefault(BaseTypeSpecifier{ node.name });
	}

	void TypeResolverVisitor::Visit(const UnaryNode& node)
	{
		CopyVisitor::Visit(node);

		const auto& unary_node = _current_clone->As<UnaryNode>();
		if (!unary_node.expression) {
			_current_clone = ErrorNode::Create("[INTERNAL] UnaryNode does not contain expression (nullptr)");
			return;
		}

		const auto result_type = GetTypeForOperator(unary_node.op, std::array{ unary_node.expression->type });
		if (result_type == Type{}) {
			_current_clone = ErrorNode::Create(std::format("operator ('{}') does not exist for type '{}'",
			                                               ASTNode::Name(unary_node.op),
			                                               std::string(unary_node.expression->type)));
			return;
		}
		_current_clone->type = result_type;
	}

	void TypeResolverVisitor::Visit(const VariableDeclarationNode& node)
	{
		CopyVisitor::Visit(node);

		if (GetVariableType(node.name)) {
			_current_clone
				= ErrorNode::Create(std::format("variable declaration '{}' shadows previous one", node.name));
			return;
		}

		_current_clone->type = GetTypeOrDefault(node.type_specifier);
		_variables_in_scope.emplace_back(std::make_pair(node.name, _current_clone->type));
	}

	void TypeResolverVisitor::Visit(const WhileNode& node)
	{
		CopyVisitor::Visit(node);
		const auto& while_loop = _current_clone->As<WhileNode>();

		if (while_loop.condition) {
			const bool is_condition_bool_coercible
				= GetCastType(while_loop.condition->type, PrimitiveType::Kind::BOOLEAN) != CastNode::Type::Impossible;
			if (!is_condition_bool_coercible) {
				_current_clone = ErrorNode::Create(
					std::format("condition in while loop statement must be convertible to a '{}' type",
				                std::string(Type{ PrimitiveType::Kind::BOOLEAN })));
				return;
			}
		}
		_current_clone->type = PrimitiveType::Kind::VOID;
	}

	CastNode::Type GetCastType(const Type& from_type, const Type& to_type)
	{
		static const std::unordered_map<PrimitiveType::Kind, std::unordered_map<PrimitiveType::Kind, CastNode::Type>>
			k_cast_type = {
				{
                 PrimitiveType::Kind::BOOLEAN,
                 {
						{ PrimitiveType::Kind::BOOLEAN, CastNode::Type::Implicit },
						{ PrimitiveType::Kind::INT32, CastNode::Type::Explicit },
						{ PrimitiveType::Kind::INT64, CastNode::Type::Explicit },
						{ PrimitiveType::Kind::STRING, CastNode::Type::Explicit },
					}  //
				},
				{
                 PrimitiveType::Kind::CHAR,
                 {
						{ PrimitiveType::Kind::CHAR, CastNode::Type::Implicit },
						{ PrimitiveType::Kind::STRING, CastNode::Type::Implicit },
					}  //
				},
				{
                 PrimitiveType::Kind::FLOAT32,
                 {
						{ PrimitiveType::Kind::FLOAT32, CastNode::Type::Implicit },
						{ PrimitiveType::Kind::FLOAT64, CastNode::Type::Implicit },
						{ PrimitiveType::Kind::INT32, CastNode::Type::Explicit },
						{ PrimitiveType::Kind::INT64, CastNode::Type::Explicit },
						{ PrimitiveType::Kind::STRING, CastNode::Type::Explicit },
					}  //
				},
				{
                 PrimitiveType::Kind::FLOAT64,
                 {
						{ PrimitiveType::Kind::FLOAT32, CastNode::Type::Explicit },
						{ PrimitiveType::Kind::FLOAT64, CastNode::Type::Implicit },
						{ PrimitiveType::Kind::INT32, CastNode::Type::Explicit },
						{ PrimitiveType::Kind::INT64, CastNode::Type::Explicit },
						{ PrimitiveType::Kind::STRING, CastNode::Type::Explicit },
					}  //
				},
				{
                 PrimitiveType::Kind::INT32,
                 {
						{ PrimitiveType::Kind::BOOLEAN, CastNode::Type::Explicit },
						{ PrimitiveType::Kind::FLOAT32, CastNode::Type::Implicit },
						{ PrimitiveType::Kind::FLOAT64, CastNode::Type::Implicit },
						{ PrimitiveType::Kind::INT32, CastNode::Type::Implicit },
						{ PrimitiveType::Kind::INT64, CastNode::Type::Implicit },
						{ PrimitiveType::Kind::STRING, CastNode::Type::Explicit },
					}  //
				},
				{
                 PrimitiveType::Kind::INT64,
                 {
						{ PrimitiveType::Kind::BOOLEAN, CastNode::Type::Explicit },
						{ PrimitiveType::Kind::FLOAT32, CastNode::Type::Implicit },
						{ PrimitiveType::Kind::FLOAT64, CastNode::Type::Implicit },
						{ PrimitiveType::Kind::INT32, CastNode::Type::Explicit },
						{ PrimitiveType::Kind::INT64, CastNode::Type::Implicit },
						{ PrimitiveType::Kind::STRING, CastNode::Type::Explicit },
					}  //
				},
				{
                 PrimitiveType::Kind::STRING,
                 {
						{ PrimitiveType::Kind::FLOAT32, CastNode::Type::Explicit },
						{ PrimitiveType::Kind::FLOAT64, CastNode::Type::Explicit },
						{ PrimitiveType::Kind::INT32, CastNode::Type::Explicit },
						{ PrimitiveType::Kind::INT64, CastNode::Type::Explicit },
						{ PrimitiveType::Kind::STRING, CastNode::Type::Implicit },
					}  //
				},
        };

		// NOTE: If the types are equivalent, no casts should take place.
		if (from_type == to_type) {
			return CastNode::Type::Implicit;
		}

		if (from_type.Is<PrimitiveType>() && to_type.Is<PrimitiveType>()) {
			const auto from = from_type.As<PrimitiveType>().type;
			const auto to   = to_type.As<PrimitiveType>().type;
			// NOTE: We cant cast the types we haven't resolved yet.
			if (from == PrimitiveType::Kind::UNKNOWN || to == PrimitiveType::Kind::UNKNOWN) {
				return CastNode::Type::Impossible;
			}

			if (k_cast_type.contains(from) && k_cast_type.at(from).contains(to)) {
				return k_cast_type.at(from).at(to);
			}

			// NOTE: Assume that undefined casts are impossible.
			return CastNode::Type::Impossible;
		}

		if (from_type.Is<ArrayType>() && to_type.Is<ArrayType>()) {
			// Arrays can only be cast if their data types are castable.
			return GetCastType(from_type.As<ArrayType>().DataType(), to_type.As<ArrayType>().DataType());
		}

		if (from_type.Is<PointerType>() && to_type.Is<PointerType>()) {
			// Pointers can only be cast if their data types are castable.
			return GetCastType(from_type.As<PointerType>().DataType(), to_type.As<PointerType>().DataType());
		}

		if (from_type.Is<StructType>() || to_type.Is<StructType>()) {
			return CastNode::Type::Impossible;
		}

		// NOTE: There was a missmatch between the types.
		return CastNode::Type::Impossible;
	}

	Type TypeResolverVisitor::GetTypeOrDefault(const TypeSpecifier& type_specifier) const noexcept
	{
		const auto it{ std::ranges::find(
			_registered_types, type_specifier, &decltype(_registered_types)::value_type::first) };
		if (it != std::end(_registered_types)) {
			return it->second;
		}
		return Type{};
	}

	std::optional<Type> TypeResolverVisitor::GetVariableType(std::string_view name) const noexcept
	{
		const auto it{ std::ranges::find(
			_variables_in_scope, name, &decltype(_variables_in_scope)::value_type::first) };
		if (it == std::end(_variables_in_scope)) [[unlikely]] {
			return std::nullopt;
		}
		return it->second;
	}
}  // namespace Soul::AST::Visitors
