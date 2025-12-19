#include "AST/Visitors/Stringify.h"

namespace Soul::AST::Visitors
{
	using namespace Soul::Types;

	StringifyVisitor::StringifyVisitor(Options options) : _options(options) {}

	std::string StringifyVisitor::string() const { return _ss.str(); }

	void StringifyVisitor::Accept(const ASTNode::Reference node)
	{
		if (!node) {
			_ss << "null";
			return;
		}

		_indent_level += k_indent_amount;
		_ss << "{\n";

		node->Accept(*this);

		_indent_level -= k_indent_amount;
		_ss << std::format("\n{}}}", std::string(_indent_level, ' '));
	}

	void StringifyVisitor::Visit(const BinaryNode& node)
	{
		Encode("node", "binary");
		EncodeType(node.type);
		Encode("operator", ASTNode::NameInternal(node.op));
		Encode("lhs", node.lhs.get());
		Encode("rhs", node.rhs.get(), false);
	}

	void StringifyVisitor::Visit(const BlockNode& node)
	{
		Encode("node", "scope_block");
		EncodeType(node.type);
		Encode("statements", node.statements, false);
	}

	void StringifyVisitor::Visit(const CastNode& node)
	{
		Encode("node", "cast");
		EncodeType(node.type);
		Encode("type_specifier", std::string(node.type_specifier));
		Encode("expression", node.expression.get(), false);
	}

	void StringifyVisitor::Visit(const ErrorNode& node)
	{
		Encode("node", "error");
		EncodeType(node.type);
		Encode("message", node.message, false);
	}

	void StringifyVisitor::Visit(const ForLoopNode& node)
	{
		Encode("node", "for_loop");
		EncodeType(node.type);
		Encode("initialization", node.initialization.get());
		Encode("condition", node.condition.get());
		Encode("update", node.update.get());
		Encode("statements", node.statements.get(), false);
	}

	void StringifyVisitor::Visit(const ForeachLoopNode& node)
	{
		Encode("node", "foreach_loop");
		EncodeType(node.type);
		Encode("variable", node.variable.get());
		Encode("in_expression", node.in_expression.get());
		Encode("statements", node.statements.get(), false);
	}

	void StringifyVisitor::Visit(const FunctionCallNode& node)
	{
		Encode("node", "function_call");
		EncodeType(node.type);
		Encode("name", node.name);
		Encode("parameters", node.parameters, false);
	}

	void StringifyVisitor::Visit(const FunctionDeclarationNode& node)
	{
		Encode("node", "function_declaration");
		EncodeType(node.type);
		Encode("name", node.name);
		Encode("type_specifier", std::string(node.type_specifier));
		Encode("parameters", node.parameters);
		Encode("statements", node.statements.get(), false);
	}

	void StringifyVisitor::Visit(const IfNode& node)
	{
		Encode("node", "if");
		EncodeType(node.type);
		Encode("expression", node.condition.get());
		Encode("then_statements", node.then_statements.get());
		Encode("else_statements", node.else_statements.get(), false);
	}

	void StringifyVisitor::Visit(const LiteralNode& node)
	{
		Encode("node", "literal");
		EncodeType(node.type);
		Encode("value_type", Value::NameInternal(node.value));
		Encode("value", std::string(node), false);
	}

	void StringifyVisitor::Visit(const LoopControlNode& node)
	{
		Encode("node", "loop_control");
		EncodeType(node.type);
		Encode("control_type",
		       node.control_type == LoopControlNode::Type::BREAK      ? "break"
		       : node.control_type == LoopControlNode::Type::CONTINUE ? "continue"
		                                                              : k_unnamed,
		       false);
	}

	void StringifyVisitor::Visit(const ModuleNode& node)
	{
		Encode("node", "module_declaration");
		EncodeType(node.type);
		Encode("name", node.name);
		Encode("statements", node.statements, false);
	}

	void StringifyVisitor::Visit(const ReturnNode& node)
	{
		Encode("node", "return");
		EncodeType(node.type);
		Encode("expression", node.expression.get(), false);
	}

	void StringifyVisitor::Visit(const StructDeclarationNode& node)
	{
		Encode("node", "struct_declaration");
		EncodeType(node.type);
		Encode("name", node.name);
		Encode("parameters", node.parameters, false);
	}

	void StringifyVisitor::Visit(const UnaryNode& node)
	{
		Encode("node", "unary");
		EncodeType(node.type);
		Encode("operator", ASTNode::NameInternal(node.op));
		Encode("expression", node.expression.get(), false);
	}

	void StringifyVisitor::Visit(const VariableDeclarationNode& node)
	{
		Encode("node", "variable_declaration");
		EncodeType(node.type);
		Encode("name", node.name);
		Encode("type_specifier", std::string(node.type_specifier));
		Encode("is_mutable", node.is_mutable ? "true" : "false");
		Encode("expression", node.expression.get(), false);
	}

	void StringifyVisitor::Visit(const WhileNode& node)
	{
		Encode("node", "while_loop");
		EncodeType(node.type);
		Encode("condition", node.condition.get());
		Encode("statements", node.statements.get(), false);
	}

	std::string StringifyVisitor::CurrentIndent() const { return std::string(_indent_level, ' '); }

	void StringifyVisitor::Encode(std::string_view key, std::string_view value, bool add_trailing_comma)
	{
		_ss << CurrentIndent();
		_ss << std::format("\"{}\": \"{}\"", key, !value.empty() ? value : k_unnamed);
		if (add_trailing_comma) {
			_ss << ",\n";
		}
	}

	void StringifyVisitor::EncodeType(const Type& type)
	{
		if (!(_options & Options::PRINT_TYPES)) {
			return;
		}
		_ss << CurrentIndent();
		_ss << std::format("\"type\": \"{}\",\n", std::string(type));
	}

	void StringifyVisitor::Encode(std::string_view key, const ASTNode::Reference node, bool add_trailing_comma)
	{
		_ss << CurrentIndent();
		_ss << std::format("\"{}\": ", key);
		Accept(node);
		if (add_trailing_comma) {
			_ss << ",\n";
		}
	}
}  // namespace Soul::AST::Visitors
