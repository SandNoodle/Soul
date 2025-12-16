#pragma once
namespace Soul::ir
{
	constexpr Function::Function(std::string_view name, types::Type return_type, std::vector<types::Type> parameters)
		: name(std::string(name)), return_type(std::move(return_type)), parameters(std::move(parameters))
	{
	}

	constexpr Module::Module(std::string_view name) : name(std::string(name)) {}
}  // namespace Soul::ir
