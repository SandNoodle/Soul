#pragma once

#include "soul.h"
#include "token.h"
#include "value.h"

typedef struct soul_ast_expression_t soul_ast_expression_t;
typedef struct soul_ast_statement_t soul_ast_statement_t;

typedef struct {
	const char* name;
	size_t length;
} soul_ast_identifier_t;

SOUL_VECTOR_DEFINE(ast_statement, soul_ast_statement_t*)

typedef enum {
	AST_EXPR_ASSIGN,
	AST_EXPR_BINARY,
	AST_EXPR_UNARY,
	AST_EXPR_VAR_LITERAL,
	AST_EXPR_BOOL_LITERAL,
	AST_EXPR_NUMBER_LITERAL,
	AST_EXPR_STRING_LITERAL,
} soul_ast_expression_type_t;

struct soul_ast_expression_t {
	uint32_t line;
	soul_ast_expression_type_t type;
	union {
		struct {
			soul_ast_expression_t* lval;
			soul_ast_expression_t* rval;
		} assign_expr;
		struct {
			soul_token_type_t op;
			soul_ast_expression_t* lval;
			soul_ast_expression_t* rval;
		} binary_expr;
		struct {
			soul_token_type_t op;
			soul_ast_expression_t* expr;
		} unary_expr;
		struct {
			soul_ast_identifier_t* id;
		} var_literal_expr;
		struct {
			soul_value_t val;
		} number_literal_expr;
		struct {
			bool val;
		} bool_literal_expr;
		struct {
			const char* str;
			size_t size;
		} string_literal_expr;
	} as;
};

typedef enum {
	AST_STMT_IF,
	AST_STMT_FOR,
	AST_STMT_FOREACH,
	AST_STMT_WHILE,
	AST_STMT_BLOCK,
	AST_STMT_RETURN,
	AST_STMT_IMPORT,
	AST_STMT_VARIABLE_DECL,
	AST_STMT_FUNCTION_DECL,
	AST_STMT_NATIVE_DECL,
	AST_STMT_DEFINE_DECL,
	AST_STMT_EXPRESSION,
} soul_ast_statement_type_t;

struct soul_ast_statement_t {
	uint32_t line;
	soul_ast_statement_type_t type;
	union {
		// Declarations
		union {
			struct {
				soul_ast_identifier_t* id;
				soul_ast_identifier_t* type;
				soul_ast_expression_t* val;
				bool is_mut;
			} var_decl;
			struct {
				soul_ast_identifier_t* id;
				soul_ast_statement_vector_t* params;
				soul_ast_identifier_t* return_type;
				soul_ast_statement_t* body;
			} fun_decl;
			struct {
				soul_ast_identifier_t* id;
				// @TODO
			} native_decl;
			struct {
				soul_ast_identifier_t* id;
				soul_ast_expression_t* val;
			} define_decl;
		} decl_stmt;
		// Control flow
		struct {
			soul_ast_expression_t* condition;
			soul_ast_statement_t* then_stmt;
			soul_ast_statement_t* else_stmt;
		} if_stmt;
		struct {
			soul_ast_statement_t* var;
			soul_ast_expression_t* condition;
			soul_ast_expression_t* actual;
			soul_ast_statement_t* body;
		} for_stmt;
		struct {
			// @TODO
		} foreach_stmt;
		struct {
			soul_ast_expression_t* condition;
			soul_ast_statement_t* body;
		} while_stmt;
		struct {
			soul_ast_statement_vector_t* stmts;
		} block_stmt;
		struct {
			soul_ast_expression_t* return_expression;
		} return_stmt;
		struct {
			// @TODO
		} import_stmt;
		struct {
			soul_ast_expression_t* expr;
		} expression_stmt;
	} as;
};

struct soul_ast_t {
	soul_ast_statement_t* root;
	bool valid;
};


// @TEMP
void soul__ast_print(soul_ast_t* ast);
//

//
// Identifiers
//

void soul__ast_free_identifier(soul_ast_identifier_t* i);

soul_ast_identifier_t* soul__ast_new_identifier(const char* name, size_t length);

//
// Expressions
//

void soul__ast_free_expression(soul_ast_expression_t* expression);

soul_ast_expression_t* soul__ast_new_expression(soul_ast_expression_type_t type,
	uint32_t line);

soul_ast_expression_t* soul__ast_assign_expression(soul_ast_expression_t* lval,
	soul_ast_expression_t* rval, uint32_t line);

soul_ast_expression_t* soul__ast_binary_expression(soul_ast_expression_t* left,
	soul_ast_expression_t* right, soul_token_type_t op, uint32_t line);

soul_ast_expression_t* soul__ast_unary_expression(soul_ast_expression_t* expr,
	soul_token_type_t op, uint32_t line);

soul_ast_expression_t* soul__ast_variable_literal_expression(const char* start,
	size_t length, uint32_t line);

soul_ast_expression_t* soul__ast_number_literal_expression(soul_value_t value,
	uint32_t line);

soul_ast_expression_t* soul__ast_bool_literal_expression(bool value, uint32_t line);

soul_ast_expression_t* soul__ast_string_literal_expression(const char* str, size_t size,
	uint32_t line);

//
// Statements
//

void soul__ast_free_statement(soul_ast_statement_t* s);

soul_ast_statement_t* soul__ast_new_statement(soul_ast_statement_type_t type,
	uint32_t line);

soul_ast_statement_t* soul__ast_new_declaration(soul_ast_statement_type_t type,
	uint32_t line);

soul_ast_statement_t* soul__ast_variable_declaration(soul_ast_identifier_t* id,
	soul_ast_identifier_t* type, soul_ast_expression_t* init, bool is_mut, uint32_t line);

soul_ast_statement_t* soul__ast_function_declaration(soul_ast_identifier_t* id,
	soul_ast_statement_vector_t* params, soul_ast_identifier_t* return_type,
	soul_ast_statement_t* body, uint32_t line);

soul_ast_statement_t* soul__ast_define_declaration(soul_ast_identifier_t* id,
	soul_ast_expression_t* val, uint32_t line);

soul_ast_statement_t* soul__ast_if_statement(soul_ast_expression_t* condition,
	soul_ast_statement_t* then_stmt, soul_ast_statement_t * else_stmt, uint32_t line);

soul_ast_statement_t* soul__ast_for_statement(soul_ast_statement_t* var,
	soul_ast_expression_t* condition, soul_ast_expression_t* actual,
	soul_ast_statement_t* body, uint32_t line);

soul_ast_statement_t* soul__ast_while_statement(soul_ast_expression_t* condition,
	soul_ast_statement_t* body, uint32_t line);

soul_ast_statement_t* soul__ast_block_statement(soul_ast_statement_vector_t* stmts,
	uint32_t line);

soul_ast_statement_t* soul__ast_return_statement(soul_ast_expression_t* expr,
	uint32_t line);

soul_ast_statement_t* soul__ast_expression_statement(soul_ast_expression_t* expr,
	uint32_t line);
