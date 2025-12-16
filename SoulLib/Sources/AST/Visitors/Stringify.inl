#pragma once
namespace Soul::ast::visitors
{
	template <std::ranges::forward_range T>
	auto StringifyVisitor::encode(std::string_view key, const T& parameters, bool add_trailing_comma) -> void
		requires(std::same_as<ASTNode::Dependency, std::ranges::range_value_t<T>>)
	{
		_ss << current_indent();

		if (parameters.empty()) {
			_ss << std::format("\"{}\": []", key);
		} else {
			_indent_level += k_indent_amount;

			_ss << std::format("\"{}\": [\n", key);
			_ss << current_indent();
			for (std::size_t index = 0; index < parameters.size(); ++index) {
				accept(parameters[index].get());
				if (index != parameters.size() - 1) {
					_ss << std::format(",\n{}", current_indent());
				}
			}

			_indent_level -= k_indent_amount;
			_ss << std::format("\n{}]", current_indent());
		}

		if (add_trailing_comma) {
			_ss << ",\n";
		}
	}

}  // namespace Soul::ast::visitors
