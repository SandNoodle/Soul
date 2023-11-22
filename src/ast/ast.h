#ifndef SOUL_AST_AST_H
#define SOUL_AST_AST_H

#include <stdbool.h>
#include <stdint.h>

typedef struct soul_ast_node_t soul_ast_node_t;

typedef struct soul_ast_node_identifier_t soul_ast_node_identifier_t;
struct soul_ast_node_identifier_t
{
	const char* str;
	uint32_t length;
};

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
		} stmt_variable_decl;
		struct {
			soul_ast_node_identifier_t id;
			soul_ast_node_t* body;
			soul_ast_node_t** params;
			uint32_t param_count;
		} stmt_function_decl;
		struct {
			soul_ast_node_t* condition;
			soul_ast_node_t* then_body;
			soul_ast_node_t* else_body; // Can be null.
		} stmt_if;
		struct {
			soul_ast_node_t* intializer;     // Can be null.
			soul_ast_node_t* condition;      // Can be null.
			soul_ast_node_t* increment_stmt; // Can be null.
		} stmt_for;
		struct {
			soul_ast_node_t* condition;
			soul_ast_node_t* body;
		} stmt_while;
		struct {
			soul_ast_node_t** statements;
			uint32_t count;
		} stmt_block;
		struct {
			soul_ast_node_t* expr; // Can be null.
		} stmt_return;
	} as;
};

/** Creates new AST Node. */
soul_ast_node_t* soul_ast_node_create();


typedef struct soul_ast_node_array_t soul_ast_node_array_t;

/** */
soul_ast_node_array_t* soul_ast_node_array_create(void);

/** */
void soul_ast_node_array_destroy(soul_ast_node_array_t*);

/** */
void soul_ast_node_array_append(soul_ast_node_array_t*, soul_ast_node_t*);

#endif // SOUL_AST_AST_H
