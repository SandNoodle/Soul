#ifndef SOUL_AST_AST_H
#define SOUL_AST_AST_H

#include <stdbool.h>
#include <stdint.h>

enum soul_token_type_t : uint8_t;
typedef struct soul_ast_node_t soul_ast_node_t;

typedef struct soul_ast_node_identifier_t soul_ast_node_identifier_t;
struct soul_ast_node_identifier_t
{
	char* str;
	uint32_t length;
};

/** Creates AST Identifier node from a string. */
soul_ast_node_identifier_t soul_ast_node_identifier_create(const char* string,
														   size_t size);

/** Destroys given AST Identifier node. */
void soul_ast_node_identifier_destroy(soul_ast_node_identifier_t* id);

typedef struct soul_ast_node_array_t soul_ast_node_array_t;
struct soul_ast_node_array_t
{
	soul_ast_node_t** nodes;
	size_t size;
	size_t capacity;
};

/** Initializes given AST Node array. */
void soul_ast_node_array_initialize(soul_ast_node_array_t* arr);

/** Appends given node to the end of the array. */
void soul_ast_node_array_append(soul_ast_node_array_t* arr,
								soul_ast_node_t* node);

/** Clears the whole array and frees nodes. */
void soul_ast_node_array_clear(soul_ast_node_array_t* arr);

/**
 * Represents operator used with unary, binary, ... nodes.
 */
typedef enum soul_ast_node_operator_t : uint8_t
{
	soul_ast_op_none, // No operator.

	// Arithmetic
	soul_ast_op_add, // Addition
	soul_ast_op_sub, // Subtraction
	soul_ast_op_mul, // Multiplication
	soul_ast_op_div, // Division
	soul_ast_op_mod, // Modulo
	soul_ast_op_inc, // Increment
	soul_ast_op_dec, // Decrement
	soul_ast_op_pow, // Power

	// Comparison
	soul_ast_op_equal,         // Equal
	soul_ast_op_not_equal,     // Not equal
	soul_ast_op_greater,       // Greater
	soul_ast_op_greater_equal, // Greater or equal
	soul_ast_op_less,          // Less
	soul_ast_op_less_equal,    // Less or equal

	// Logical
	soul_ast_op_logic_not, // Negation
	soul_ast_op_logic_and, // And
	soul_ast_op_logic_or,  // Or
} soul_ast_node_operator_t;

soul_ast_node_operator_t soul_token_type_to_operator(enum soul_token_type_t type);

/**
 * Represents underlying type of an AST Node.
 */
typedef enum soul_ast_node_type_t : uint8_t
{
	// Expressions
	soul_ast_expr_assign,
	soul_ast_expr_unary,
	soul_ast_expr_binary,
	soul_ast_expr_var_literal,
	soul_ast_expr_bool_literal,
	soul_ast_expr_number_literal,
	soul_ast_expr_string_literal,
	soul_ast_expr_stmt,

	// Statements
	soul_ast_stmt_if,
	soul_ast_stmt_for,
	soul_ast_stmt_while,
	soul_ast_stmt_block,
	soul_ast_stmt_return,
	soul_ast_stmt_variable_decl,
	soul_ast_stmt_function_decl,
	soul_ast_stmt_native_decl,
} soul_ast_node_type_t;

/**
 * Represents a single Node in an AST.
 *
 */
struct soul_ast_node_t {
	soul_ast_node_type_t type;
	union {
		// Expressions
		struct {
			soul_ast_node_t* lhs;
			soul_ast_node_t* rhs;
		} expr_assign;
		struct {
			soul_ast_node_operator_t op;
			soul_ast_node_t* expr;
		} expr_unary;
		struct {
			soul_ast_node_operator_t op;
			soul_ast_node_t* lhs;
			soul_ast_node_t* rhs;
		} expr_binary;
		struct {
			soul_ast_node_identifier_t id;
		} expr_literal_variable;
		struct {
			bool val;
		} expr_literal_bool;
		struct {
			int64_t val;
		} expr_literal_number;
		struct {
			soul_ast_node_identifier_t val;
		} expr_literal_string;
		struct {
			soul_ast_node_t* stmt;
		} expr_stmt;

		// Statements
		struct {
			soul_ast_node_identifier_t id;
			soul_ast_node_identifier_t type;
			soul_ast_node_t* expr;
			bool is_mutable;
		} stmt_variable_decl;
		struct {
			soul_ast_node_identifier_t id;
			soul_ast_node_identifier_t type;
			soul_ast_node_array_t params;
			soul_ast_node_t* body;
		} stmt_function_decl;
		struct {
			soul_ast_node_t* condition;
			soul_ast_node_t* then_body;
			soul_ast_node_t* else_body; // Can be null.
		} stmt_if;
		struct {
			soul_ast_node_t* initializer;    // Can be null.
			soul_ast_node_t* condition;      // Can be null.
			soul_ast_node_t* increment_stmt; // Can be null.
			soul_ast_node_t* body;
		} stmt_for;
		struct {
			soul_ast_node_t* condition;
			soul_ast_node_t* body;
		} stmt_while;
		struct {
			soul_ast_node_array_t stmts;
		} stmt_block;
		struct {
			soul_ast_node_t* expr; // Can be null.
		} stmt_return;
	} as;
};

/** Creates new AST Node. */
soul_ast_node_t* soul_ast_node_create(soul_ast_node_type_t);

/** Destroys given AST Node. */
void soul_ast_node_destroy(soul_ast_node_t* node);

soul_ast_node_t* soul_ast_node_create_assign_expression(soul_ast_node_t* lhs,
														soul_ast_node_t* rhs);
soul_ast_node_t* soul_ast_node_create_unary_expression(soul_ast_node_t* expr,
													   soul_ast_node_operator_t op);
soul_ast_node_t* soul_ast_node_create_binary_expression(soul_ast_node_t* lhs,
														soul_ast_node_t* rhs,
														soul_ast_node_operator_t op);
soul_ast_node_t* soul_ast_node_create_variable_literal_expression(
		soul_ast_node_identifier_t identifier);
soul_ast_node_t* soul_ast_node_create_boolean_literal_expression(bool val);
soul_ast_node_t* soul_ast_node_create_number_literal_expression(int64_t val);
soul_ast_node_t* soul_ast_node_create_string_literal_expression(
		soul_ast_node_identifier_t val);
soul_ast_node_t* soul_ast_node_create_expression_statement(
		soul_ast_node_t* stmt);

soul_ast_node_t* soul_ast_node_create_variable_decl_statement(
		soul_ast_node_identifier_t id,
		soul_ast_node_identifier_t type,
		soul_ast_node_t* expr,
		bool is_mutable);
soul_ast_node_t* soul_ast_node_create_function_decl_statement(
		soul_ast_node_identifier_t id,
		soul_ast_node_identifier_t type,
		soul_ast_node_t* body,
		soul_ast_node_array_t params);
soul_ast_node_t* soul_ast_node_create_if_statement(soul_ast_node_t* condition,
												   soul_ast_node_t* then_body,
												   soul_ast_node_t* else_body);
soul_ast_node_t* soul_ast_node_create_for_statement(
		soul_ast_node_t* initializer,
		soul_ast_node_t* condition,
		soul_ast_node_t* increment_stmt,
		soul_ast_node_t* body);
soul_ast_node_t* soul_ast_node_create_while_statement(
		soul_ast_node_t* condition,
		soul_ast_node_t* body);
soul_ast_node_t* soul_ast_node_create_block_statement(
		soul_ast_node_array_t statements);
soul_ast_node_t* soul_ast_node_create_return_statement(
		soul_ast_node_t* return_expr);

#endif // SOUL_AST_AST_H
