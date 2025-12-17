#include "AST/Visitors/Stringify.h"

namespace soul::ast::visitors
{
	using namespace soul::types;

	StringifyVisitor::StringifyVisitor(Options options) : _options(options) {}

	std::string StringifyVisitor::string() const { return _ss.str(); }

	void StringifyVisitor::accept(const ASTNode::Reference node)
	{
		if (!node) {
			_ss << "null";
			return;
		}

		_indent_level += k_indent_amount;
		_ss << "{\n";

		node->accept(*this);

		_indent_level -= k_indent_amount;
		_ss << std::format("\n{}}}", std::string(_indent_level, ' '));
	}

	void StringifyVisitor::visit(const BinaryNode& node)
	{
		encode("node", "binary");
		encode_type(node.type);
		encode("operator", ASTNode::internal_name(node.op));
		encode("lhs", node.lhs.get());
		encode("rhs", node.rhs.get(), false);
	}

	void StringifyVisitor::visit(const BlockNode& node)
	{
		encode("node", "scope_block");
		encode_type(node.type);
		encode("statements", node.statements, false);
	}

	void StringifyVisitor::visit(const CastNode& node)
	{
		encode("node", "cast");
		encode_type(node.type);
		encode("type_specifier", std::string(node.type_specifier));
		encode("expression", node.expression.get(), false);
	}

	void StringifyVisitor::visit(const ErrorNode& node)
	{
		encode("node", "error");
		encode_type(node.type);
		encode("message", node.message, false);
	}

	void StringifyVisitor::visit(const ForLoopNode& node)
	{
		encode("node", "for_loop");
		encode_type(node.type);
		encode("initialization", node.initialization.get());
		encode("condition", node.condition.get());
		encode("update", node.update.get());
		encode("statements", node.statements.get(), false);
	}

	void StringifyVisitor::visit(const ForeachLoopNode& node)
	{
		encode("node", "foreach_loop");
		encode_type(node.type);
		encode("variable", node.variable.get());
		encode("in_expression", node.in_expression.get());
		encode("statements", node.statements.get(), false);
	}

	void StringifyVisitor::visit(const FunctionCallNode& node)
	{
		encode("node", "function_call");
		encode_type(node.type);
		encode("name", node.name);
		encode("parameters", node.parameters, false);
	}

	void StringifyVisitor::visit(const FunctionDeclarationNode& node)
	{
		encode("node", "function_declaration");
		encode_type(node.type);
		encode("name", node.name);
		encode("type_specifier", std::string(node.type_specifier));
		encode("parameters", node.parameters);
		encode("statements", node.statements.get(), false);
	}

	void StringifyVisitor::visit(const IfNode& node)
	{
		encode("node", "if");
		encode_type(node.type);
		encode("expression", node.condition.get());
		encode("then_statements", node.then_statements.get());
		encode("else_statements", node.else_statements.get(), false);
	}

	void StringifyVisitor::visit(const LiteralNode& node)
	{
		encode("node", "literal");
		encode_type(node.type);
		encode("value_type", Value::internal_name(node.value));
		encode("value", std::string(node), false);
	}

	void StringifyVisitor::visit(const LoopControlNode& node)
	{
		encode("node", "loop_control");
		encode_type(node.type);
		encode("control_type",
		       node.control_type == LoopControlNode::Type::Break      ? "break"
		       : node.control_type == LoopControlNode::Type::Continue ? "continue"
		                                                              : k_unnamed,
		       false);
	}

	void StringifyVisitor::visit(const ModuleNode& node)
	{
		encode("node", "module_declaration");
		encode_type(node.type);
		encode("name", node.name);
		encode("statements", node.statements, false);
	}

	void StringifyVisitor::visit(const ReturnNode& node)
	{
		encode("node", "return");
		encode_type(node.type);
		encode("expression", node.expression.get(), false);
	}

	void StringifyVisitor::visit(const StructDeclarationNode& node)
	{
		encode("node", "struct_declaration");
		encode_type(node.type);
		encode("name", node.name);
		encode("parameters", node.parameters, false);
	}

	void StringifyVisitor::visit(const UnaryNode& node)
	{
		encode("node", "unary");
		encode_type(node.type);
		encode("operator", ASTNode::internal_name(node.op));
		encode("expression", node.expression.get(), false);
	}

	void StringifyVisitor::visit(const VariableDeclarationNode& node)
	{
		encode("node", "variable_declaration");
		encode_type(node.type);
		encode("name", node.name);
		encode("type_specifier", std::string(node.type_specifier));
		encode("is_mutable", node.is_mutable ? "true" : "false");
		encode("expression", node.expression.get(), false);
	}

	void StringifyVisitor::visit(const WhileNode& node)
	{
		encode("node", "while_loop");
		encode_type(node.type);
		encode("condition", node.condition.get());
		encode("statements", node.statements.get(), false);
	}

	std::string StringifyVisitor::current_indent() const { return std::string(_indent_level, ' '); }

	void StringifyVisitor::encode(std::string_view key, std::string_view value, bool add_trailing_comma)
	{
		_ss << current_indent();
		_ss << std::format("\"{}\": \"{}\"", key, !value.empty() ? value : k_unnamed);
		if (add_trailing_comma) {
			_ss << ",\n";
		}
	}

	void StringifyVisitor::encode_type(const Type& type)
	{
		if (!(_options & Options::PrintTypes)) {
			return;
		}
		_ss << current_indent();
		_ss << std::format("\"type\": \"{}\",\n", std::string(type));
	}

	void StringifyVisitor::encode(std::string_view key, const ASTNode::Reference node, bool add_trailing_comma)
	{
		_ss << current_indent();
		_ss << std::format("\"{}\": ", key);
		accept(node);
		if (add_trailing_comma) {
			_ss << ",\n";
		}
	}
}  // namespace soul::ast::visitors
