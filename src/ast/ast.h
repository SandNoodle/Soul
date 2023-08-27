#ifndef SOUL_AST_AST_H
#define SOUL_AST_AST_H

#include "ast/ast_node_types.h"

#include <vector>

namespace soul
{
	enum class token_type : uint8_t;

	/**
	 * Represents operator used with unary, binary, ... nodes.
	 */
	enum class ast_node_operator : uint8_t
	{
		op_none, // No operator.

		// Arithmetic
		op_add, // Addition
		op_sub, // Subtraction
		op_mul, // Multiplication
		op_div, // Division
		op_mod, // Modulo
		/* op_inc, // Increment */
		/* op_dec, // Decrement */
		op_pow, // Power

		// Comparison
		op_equal,         // Equal
		op_not_equal,     // Not equal
		op_greater,       // Greater
		op_greater_equal, // Greater or equal
		op_less,          // Less
		op_less_equal,    // Less or equal

		// Logical
		op_logic_not, // Negation
		op_logic_and, // And
		op_logic_or,  // Or
	};

	/**
	 * Represents underlying type of an AST Node.
	 */
	enum class ast_node_type : uint8_t
	{
		// Expressions
		ast_expr_assign,
		ast_expr_binary,
		ast_expr_unary,
		ast_expr_var_literal,
		ast_expr_bool_literal,
		ast_expr_number_literal,
		ast_expr_string_literal,
		ast_expr_stmt,

		// Statements
		ast_stmt_if,
		ast_stmt_for,
		ast_stmt_foreach,
		ast_stmt_while,
		ast_stmt_block,
		ast_stmt_return,
		ast_stmt_import,
		ast_stmt_variable_decl,
		ast_stmt_function_decl,
		ast_stmt_native_decl,
		ast_stmt_define_decl,
	};

	/**
	 * Represents a single Node in an AST.
	 *
	 */
	struct ast_node {
		ast_node_type type;
		union {
			// Expressions
			ast_node_assign         assign_expr;
			ast_node_binary         binary_expr;
			ast_node_unary          unary_expr;
			ast_node_var_literal    var_lit_expr;
			ast_node_bool_literal   bool_lit_expr;
			ast_node_number_literal number_lit_expr;
			ast_node_string_literal string_lit_expr;
			ast_node_expr_stmt      expr_stmt;

			// Statements
			ast_node_decl_variable variable_decl_stmt;
			ast_node_decl_function function_decl_stmt;
			ast_node_decl_native   native_decl_stmt;
			ast_node_decl_define   define_decl_stmt;
			ast_node_if            if_stmt;
			ast_node_for           for_stmt;
			ast_node_while         while_stmt;
			ast_node_block         block_stmt;
			ast_node_return        return_stmt;
		} as;
	};

	ast_node_operator to_node_operator(token_type type);
} // namespace soul

#endif // SOUL_AST_AST_H
