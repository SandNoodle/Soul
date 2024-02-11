#include "ast.h"

#include <stdio.h> // @TODO TEMP
#include <stdlib.h>
#include <string.h>

#include "token.h"

// @TODO: Move out into a config file.
#define SOUL_AST_NODE_ARRAY_MIN_CAPACITY 8

bool soul_is_unary_operator(soul_ast_node_operator_t op)
{
	return op == soul_ast_op_sub
		|| op == soul_ast_op_inc
		|| op == soul_ast_op_dec
		|| op == soul_ast_op_logic_not;
}

bool soul_is_binary_operator(soul_ast_node_operator_t op)
{
	return op == soul_ast_op_add
		|| op == soul_ast_op_sub
		|| op == soul_ast_op_mul
		|| op == soul_ast_op_div
		|| op == soul_ast_op_mod
		|| op == soul_ast_op_pow
		|| op == soul_ast_op_equal
		|| op == soul_ast_op_not_equal
		|| op == soul_ast_op_greater
		|| op == soul_ast_op_greater_equal
		|| op == soul_ast_op_less
		|| op == soul_ast_op_less_equal
		|| op == soul_ast_op_logic_and
		|| op == soul_ast_op_logic_or;
}

const char* soul_ast_node_type_to_string(soul_ast_node_type_t type)
{
	// IMPORTANT: Has to match soul_ast_node_type_t
	// clang-format off
	const char* names[] = {
		[soul_ast_expr_assign]         = "assign",
		[soul_ast_expr_unary]          = "unary",
		[soul_ast_expr_binary]         = "binary",
		[soul_ast_expr_var_literal]    = "variable_literal",
		[soul_ast_expr_bool_literal]   = "boolean_literal",
		[soul_ast_expr_number_literal] = "number_literal",
		[soul_ast_expr_string_literal] = "string_literal",
		[soul_ast_expr_stmt]           = "expression_statement",
		[soul_ast_stmt_variable_decl]  = "variable_declaration_statement",
		[soul_ast_stmt_function_decl]  = "function_declaration_statement",
		[soul_ast_stmt_native_decl]    = "native_declaration_statement",
		[soul_ast_stmt_if]             = "if_statement",
		[soul_ast_stmt_for]            = "for_statement",
		[soul_ast_stmt_while]          = "while_statement",
		[soul_ast_stmt_block]          = "block_statement",
		[soul_ast_stmt_return]         = "return_statement",
	};
	// clang-format on
	return names[type];
}

soul_ast_node_identifier_t soul_ast_node_identifier_create(const char* string,
                                                           size_t length,
                                                           soul_allocator_t* allocator)
{
	soul_ast_node_identifier_t id;
	id.length = length;
	id.data   = (char*)allocator->alloc(sizeof(char) * id.length + 1, allocator->user_data);
	memcpy(id.data, string, id.length);
	id.data[id.length] = '\0';
	return id;
}

void soul_ast_node_identifier_destroy(soul_ast_node_identifier_t* id, soul_allocator_t* allocator)
{
	if (!id) return;
	allocator->free(id->data, sizeof(char) * id->length + 1, allocator->user_data);
	id->data   = NULL;
	id->length = 0;
}

soul_ast_node_array_t soul_ast_node_array_create(soul_allocator_t* allocator)
{
	soul_ast_node_array_t array;
	array.size     = 0;
	array.capacity = SOUL_AST_NODE_ARRAY_MIN_CAPACITY;
	array.nodes = (soul_ast_node_t**)allocator->alloc(sizeof(soul_ast_node_t*) * array.capacity, allocator->user_data);
}

void soul_ast_node_array_destroy(soul_ast_node_array_t* array, soul_allocator_t* allocator)
{
	if(!array) return;
	allocator->free(array->nodes, sizeof(soul_ast_node_t) * array->capacity, allocator->user_data);
	array->nodes     = NULL;
	array->size      = 0;
	array->capacity  = 0;
}

void soul_ast_node_array_append(soul_ast_node_array_t* arr,
                                soul_ast_node_t* node,
                                soul_allocator_t* allocator)
{
	if (!arr || !node) return;
	if (arr->size + 1 > arr->capacity)
	{
		const size_t new_capacity = arr->capacity >= 8 ? arr->capacity * 2 : 8;
		arr->capacity             = new_capacity;
		arr->nodes = (soul_ast_node_t**)allocator->realloc(
		    arr->nodes, sizeof(soul_ast_node_t*) * new_capacity, allocator->user_data);
	}
	arr->nodes[arr->size++] = node;
}

void soul_ast_node_array_clear(soul_ast_node_array_t* arr, soul_allocator_t* allocator)
{
	if (!arr) return;
	for (size_t i = 0; i < arr->size; ++i)
	{
		soul_ast_node_destroy(arr->nodes[i], allocator);
	}
}

soul_ast_node_operator_t soul_token_type_to_operator(
    enum soul_token_type_t type)
{
	switch (type)
	{
		case soul_token_plus_equal:
		case soul_token_plus:
			return soul_ast_op_add;
		case soul_token_minus:
		case soul_token_minus_equal:
			return soul_ast_op_sub;
		case soul_token_star:
		case soul_token_star_equal:
			return soul_ast_op_mul;
		case soul_token_slash:
		case soul_token_slash_equal:
			return soul_ast_op_div;
		case soul_token_percent:
			return soul_ast_op_mod;
		case soul_token_double_plus:
			return soul_ast_op_inc;
		case soul_token_double_minus:
			return soul_ast_op_dec;
		case soul_token_equal:
			return soul_ast_op_equal;
		case soul_token_bang_equal:
			return soul_ast_op_not_equal;
		case soul_token_greater:
			return soul_ast_op_greater;
		case soul_token_greater_equal:
			return soul_ast_op_greater_equal;
		case soul_token_less:
			return soul_ast_op_less;
		case soul_token_less_equal:
			return soul_ast_op_less_equal;
		case soul_token_bang:
			return soul_ast_op_logic_not;
		case soul_token_double_ampersand:
			return soul_ast_op_logic_and;
		case soul_token_double_pipe:
			return soul_ast_op_logic_or;
		default:
			break;
	}
	return soul_ast_op_none;
}

soul_ast_node_t* soul_ast_node_create(soul_ast_node_type_t type, soul_allocator_t* allocator)
{
	soul_ast_node_t* node = (soul_ast_node_t*)allocator->alloc(sizeof(soul_ast_node_t), allocator->user_data);
	node->type = type;
	return node;
}

// @TODO: Destroy
void soul_ast_node_destroy(soul_ast_node_t* node, soul_allocator_t* allocator)
{
	if (!node) return;
	switch (node->type)
	{
		case soul_ast_expr_assign:
			soul_ast_node_destroy(node->as.expr_assign.lhs, allocator);
			soul_ast_node_destroy(node->as.expr_assign.rhs, allocator);
			break;
		case soul_ast_expr_unary:
			soul_ast_node_destroy(node->as.expr_unary.expr, allocator);
			break;
		case soul_ast_expr_binary:
			soul_ast_node_destroy(node->as.expr_binary.lhs, allocator);
			soul_ast_node_destroy(node->as.expr_binary.rhs, allocator);
			break;
		case soul_ast_expr_var_literal:
			// TODO: Identifier
			break;
		case soul_ast_expr_bool_literal:
			break;
		case soul_ast_expr_number_literal:
			break;
		case soul_ast_expr_string_literal:
			// TODO: Identifier
			break;
		case soul_ast_expr_stmt:
			soul_ast_node_destroy(node->as.expr_stmt.stmt, allocator);
			break;
		case soul_ast_stmt_if:
			soul_ast_node_destroy(node->as.stmt_if.condition, allocator);
			soul_ast_node_destroy(node->as.stmt_if.then_body, allocator);
			soul_ast_node_destroy(node->as.stmt_if.else_body, allocator);
			break;
		case soul_ast_stmt_for:
			break;
		case soul_ast_stmt_while:
			break;
		case soul_ast_stmt_block:
			soul_ast_node_array_destroy(&node->as.stmt_block.stmts, allocator);
			break;
		case soul_ast_stmt_return:
			break;
		case soul_ast_stmt_variable_decl:
			break;
		case soul_ast_stmt_function_decl:
			soul_ast_node_identifier_destroy(&node->as.stmt_function_decl.id, allocator);
			soul_ast_node_identifier_destroy(&node->as.stmt_function_decl.type, allocator);
			soul_ast_node_array_destroy(&node->as.stmt_function_decl.params, allocator);
			soul_ast_node_destroy(node->as.stmt_function_decl.body, allocator);
			break;
		case soul_ast_stmt_native_decl:
			break;
		default:
			// TODO: Unhandled.
			break;
	}

	soul_ast_node_destroy(node, allocator);
}

soul_ast_node_t* soul_ast_node_create_assign_expression(soul_ast_node_t* lhs,
                                                        soul_ast_node_t* rhs,
                                                        soul_allocator_t* allocator)
{
	soul_ast_node_t* node    = soul_ast_node_create(soul_ast_expr_assign, allocator);
	node->as.expr_assign.lhs = lhs;
	node->as.expr_assign.rhs = rhs;
	return node;
}

soul_ast_node_t* soul_ast_node_create_unary_expression(
    soul_ast_node_t* expr, soul_ast_node_operator_t op, soul_allocator_t* allocator)
{
	soul_ast_node_t* node    = soul_ast_node_create(soul_ast_expr_unary, allocator);
	node->as.expr_unary.expr = expr;
	node->as.expr_unary.op   = op;
	return node;
}

soul_ast_node_t* soul_ast_node_create_binary_expression(
    soul_ast_node_t* lhs, soul_ast_node_t* rhs, soul_ast_node_operator_t op, soul_allocator_t* allocator)
{
	soul_ast_node_t* node    = soul_ast_node_create(soul_ast_expr_binary, allocator);
	node->as.expr_binary.lhs = lhs;
	node->as.expr_binary.rhs = rhs;
	node->as.expr_binary.op  = op;
	return node;
}

soul_ast_node_t* soul_ast_node_create_variable_literal_expression(
    soul_ast_node_identifier_t identifier, soul_allocator_t* allocator)
{
	soul_ast_node_t* node = soul_ast_node_create(soul_ast_expr_var_literal, allocator);
	node->as.expr_literal_variable.id = identifier;
	return node;
}

soul_ast_node_t* soul_ast_node_create_boolean_literal_expression(bool val, soul_allocator_t* allocator)
{
	soul_ast_node_t* node = soul_ast_node_create(soul_ast_expr_bool_literal, allocator);
	node->as.expr_literal_bool.val = val;
	return node;
}

soul_ast_node_t* soul_ast_node_create_number_literal_expression(int64_t val, soul_allocator_t* allocator)
{
	soul_ast_node_t* node = soul_ast_node_create(soul_ast_expr_number_literal, allocator);
	node->as.expr_literal_number.val = val;
	return node;
}

soul_ast_node_t* soul_ast_node_create_string_literal_expression(
    soul_ast_node_identifier_t val, soul_allocator_t* allocator)
{
	soul_ast_node_t* node = soul_ast_node_create(soul_ast_expr_string_literal, allocator);
	node->as.expr_literal_string.val = val;
	return node;
}

soul_ast_node_t* soul_ast_node_create_expression_statement(
    soul_ast_node_t* stmt, soul_allocator_t* allocator)
{
	soul_ast_node_t* node   = soul_ast_node_create(soul_ast_expr_stmt, allocator);
	node->as.expr_stmt.stmt = stmt;
	return node;
}

soul_ast_node_t* soul_ast_node_create_variable_decl_statement(
    soul_ast_node_identifier_t id, soul_ast_node_identifier_t type,
    soul_ast_node_t* expr, bool is_mutable, soul_allocator_t* allocator)
{
	soul_ast_node_t* node = soul_ast_node_create(soul_ast_stmt_variable_decl, allocator);
	node->as.stmt_variable_decl.id         = id;
	node->as.stmt_variable_decl.type       = type;
	node->as.stmt_variable_decl.expr       = expr;
	node->as.stmt_variable_decl.is_mutable = is_mutable;
	return node;
}

soul_ast_node_t* soul_ast_node_create_function_decl_statement(
    soul_ast_node_identifier_t id, soul_ast_node_identifier_t type,
    soul_ast_node_t* body, soul_ast_node_array_t params, soul_allocator_t* allocator)
{
	soul_ast_node_t* node = soul_ast_node_create(soul_ast_stmt_function_decl, allocator);
	node->as.stmt_function_decl.id     = id;
	node->as.stmt_function_decl.type   = type;
	node->as.stmt_function_decl.params = params;
	return node;
}

soul_ast_node_t* soul_ast_node_create_if_statement(soul_ast_node_t* condition,
                                                   soul_ast_node_t* then_body,
                                                   soul_ast_node_t* else_body, soul_allocator_t* allocator)
{
	soul_ast_node_t* node      = soul_ast_node_create(soul_ast_stmt_if, allocator);
	node->as.stmt_if.condition = condition;
	node->as.stmt_if.then_body = then_body;
	node->as.stmt_if.else_body = else_body;
	return node;
}

soul_ast_node_t* soul_ast_node_create_for_statement(
    soul_ast_node_t* initializer, soul_ast_node_t* condition,
    soul_ast_node_t* increment_stmt, soul_ast_node_t* body, soul_allocator_t* allocator)
{
	soul_ast_node_t* node            = soul_ast_node_create(soul_ast_stmt_for, allocator);
	node->as.stmt_for.initializer    = initializer;
	node->as.stmt_for.condition      = condition;
	node->as.stmt_for.increment_stmt = increment_stmt;
	node->as.stmt_for.body           = body;
	return node;
}

soul_ast_node_t* soul_ast_node_create_while_statement(
    soul_ast_node_t* condition, soul_ast_node_t* body, soul_allocator_t* allocator)
{
	soul_ast_node_t* node         = soul_ast_node_create(soul_ast_stmt_while, allocator);
	node->as.stmt_while.condition = condition;
	node->as.stmt_while.body      = body;
	return node;
}

soul_ast_node_t* soul_ast_node_create_block_statement(
    soul_ast_node_array_t statements, soul_allocator_t* allocator)
{
	soul_ast_node_t* node     = soul_ast_node_create(soul_ast_stmt_block, allocator);
	node->as.stmt_block.stmts = statements;
	return node;
}

soul_ast_node_t* soul_ast_node_create_return_statement(
    soul_ast_node_t* return_expr, soul_allocator_t* allocator)
{
	soul_ast_node_t* node     = soul_ast_node_create(soul_ast_stmt_return, allocator);
	node->as.stmt_return.expr = return_expr;
	return node;
}
