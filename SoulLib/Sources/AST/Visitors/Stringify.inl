#pragma once
namespace Soul::AST::Visitors
{
	template <std::ranges::forward_range T>
	auto StringifyVisitor::Encode(std::string_view key, const T& parameters, bool add_trailing_comma) -> void
		requires(std::same_as<ASTNode::Dependency, std::ranges::range_value_t<T>>)
	{
		_ss << CurrentIndent();

		if (parameters.empty()) {
			_ss << std::format("\"{}\": []", key);
		} else {
			_indent_level += k_indent_amount;

			_ss << std::format("\"{}\": [\n", key);
			_ss << CurrentIndent();
			for (std::size_t index = 0; index < parameters.size(); ++index) {
				Accept(parameters[index].get());
				if (index != parameters.size() - 1) {
					_ss << std::format(",\n{}", CurrentIndent());
				}
			}

			_indent_level -= k_indent_amount;
			_ss << std::format("\n{}]", CurrentIndent());
		}

		if (add_trailing_comma) {
			_ss << ",\n";
		}
	}

}  // namespace Soul::AST::Visitors
