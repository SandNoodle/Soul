#pragma once
namespace Soul::IR
{
	constexpr Function::Function(std::string_view name, Types::Type return_type, std::vector<Types::Type> parameters)
		: name(std::string(name)), return_type(std::move(return_type)), parameters(std::move(parameters))
	{
	}

	constexpr Module::Module(std::string_view name) : name(std::string(name)) {}
}  // namespace Soul::IR
