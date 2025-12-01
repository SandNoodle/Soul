#pragma once

#include "types/overloads.h"

namespace soul::ast::visitors
{
	types::Type TypeResolverVisitor::get_type_for_operator(
		ASTNode::Operator                      op,
		const std::ranges::forward_range auto& input_types) const noexcept
	{
		{
			if (op == ASTNode::Operator::Unknown) [[unlikely]] {
				return types::Type{};
			}

			auto potential_overloads{
				types::OperatorOverload::all()
				| std::views::filter([op](const auto& overload) -> bool { return overload.op == op; })
				| std::views::filter([input_count = input_types.size()](const auto& overload) -> bool {
					  return overload.input_types.size() == input_count;
				  })
				| std::views::filter([&input_types](const auto& overload) -> bool {
					  for (std::size_t index = 0; index < input_types.size(); ++index) {
						  if (input_types[index] != overload.input_types[index]) {
							  return false;
						  }
					  }
					  return true;
				  })
			};
			if (potential_overloads.empty()) {
				return types::Type{};
			}
			return potential_overloads.begin()->return_type;
		}
	}

	std::optional<TypeResolverVisitor::FunctionDeclaration> TypeResolverVisitor::get_function_declaration(
		std::string_view                       name,
		const std::ranges::forward_range auto& want_types) const noexcept
	{
		auto potential_declarations{
			_functions_in_module
			| std::views::filter([name](const auto& function) -> bool { return function.first == name; })
			| std::views::transform([](const auto& function) -> FunctionDeclaration { return function.second; })
			| std::views::filter([want_types_count = want_types.size()](const auto& function) -> bool {
				  return function.input_types.size() == want_types_count;
			  })
			| std::views::filter([&want_types](const auto& function) -> bool {
				  for (std::size_t index = 0; index < want_types.size(); ++index) {
					  if (want_types[index] != function.input_types[index]) {
						  return false;
					  }
				  }
				  return true;
			  })
		};
		if (potential_declarations.empty()) {
			return std::nullopt;
		}
		return *potential_declarations.begin();
	}
}  // namespace soul::ast::visitors
