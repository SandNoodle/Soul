#ifndef SOUL_AST_AST_NODE_TYPES_H
#define SOUL_AST_AST_NODE_TYPES_H

#include <stdint.h>
#include <string>
#include <vector>

namespace soul
{
	enum class ast_node_operator : uint8_t;
	struct ast_node;

	using ast_node_identifier = std::string;

	//
	// Expressions
	//
	struct ast_node_assign
	{
		ast_node* lhs;
		ast_node* rhs;
	};

	struct ast_node_binary
	{
		ast_node_operator op;
		ast_node* lval;
		ast_node* rval;
	};

	struct ast_node_unary
	{
		ast_node_operator op;
		ast_node* val;
	};

	struct ast_node_var_literal
	{
		std::string val;
	};

	struct ast_node_bool_literal
	{
		bool val;
	};

	struct ast_node_number_literal
	{
		enum class number_type
		{
			integer, real
		} type;
		union
		{
			int64_t int_val;
			double  real_val;
		} as;
	};

	struct ast_node_string_literal
	{
		std::string val;
	};

	struct ast_node_expr_stmt
	{
		ast_node* expr;
	};

	//
	// Statements
	//
	struct ast_node_decl_variable
	{
		ast_node_identifier identifier;
		ast_node*   val;
	};

	struct ast_node_decl_function
	{
		std::string identifier;
		ast_node*   body;
	};

	struct ast_node_decl_native
	{
		// @TODO
	};

	struct ast_node_decl_define
	{
		ast_node_identifier id;
		ast_node*      expr;
	};

	struct ast_node_if
	{
		ast_node* condition;
		ast_node* then_stmt;
		ast_node* else_stmt; // Can be null.
	};

	struct ast_node_for
	{
		ast_node* initializer; // Can be null.
		ast_node* condition;   // Can be null.
		ast_node* expr;        // Can be null.
		ast_node* body;
	};

	struct ast_node_while
	{
		ast_node* condition;
		ast_node* body;
	};

	struct ast_node_block
	{
		std::vector<ast_node*> stmts;
	};

	struct ast_node_return
	{
		ast_node* return_expr;
	};
} // namespace soul

#endif // SOUL_AST_AST_NODE_TYPES_H
