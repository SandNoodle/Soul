#include "type_checker.h"

#include "ast/ast.h"

#include <stdbool.h>
#include <string.h>

// @TODO: Implement panic mode for the type_checker so we can continue type
// checking before returning an error.

// Util
static void type_checker_register_type(soul_type_checker_t*);
static soul_type_checker_result_t type_checker_error(const char* message);

typedef enum soul_basic_type_t
{
	soul_basic_type_unknown, // Special "type" signaling an error.

	soul_basic_type_integer,
	soul_basic_type_float,
	soul_basic_type_bool,
	soul_basic_type_string,
	soul_basic_type_struct,
	soul_basic_type_enum,
} soul_basic_type_t;

soul_type_checker_t soul_type_checker_create(void)
{
	soul_type_checker_t type_checker;
	return type_checker;
}

static soul_basic_type_t soul_type_checker_compare(soul_basic_type_t first,
                                                   soul_basic_type_t second)
{
	// @TODO
	return soul_basic_type_unknown;
}

static soul_basic_type_t soul_type_checker_get_type(
    soul_type_checker_t* type_checker, soul_ast_node_identifier_t id)
{
	// @TODO
	return soul_basic_type_unknown;
}

static soul_basic_type_t soul_type_checker_traverse(
    soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	switch (node->type)
	{
		case soul_ast_expr_assign: {
			soul_basic_type_t lhs = soul_type_checker_traverse(
			    type_checker, node->as.expr_assign.lhs);
			soul_basic_type_t rhs = soul_type_checker_traverse(
			    type_checker, node->as.expr_assign.rhs);
			return soul_type_checker_compare(lhs, rhs);
		}
		case soul_ast_expr_unary:
			return soul_type_checker_traverse(type_checker,
			                                  node->as.expr_unary.expr);
		case soul_ast_expr_binary: {
			soul_basic_type_t lhs = soul_type_checker_traverse(
			    type_checker, node->as.expr_binary.lhs);
			soul_basic_type_t rhs = soul_type_checker_traverse(
			    type_checker, node->as.expr_binary.rhs);
			return soul_type_checker_compare(lhs, rhs);
		}
		case soul_ast_expr_var_literal:
			return soul_type_checker_get_type(
			    type_checker, node->as.expr_literal_variable.id);
		case soul_ast_expr_bool_literal:
			return soul_basic_type_bool;
		case soul_ast_expr_number_literal:
			return soul_basic_type_integer;
		case soul_ast_expr_string_literal:
			return soul_basic_type_string;
		case soul_ast_expr_stmt:
			return soul_type_checker_traverse(type_checker,
			                                  node->as.expr_stmt.stmt);
		case soul_ast_stmt_if: {
			soul_basic_type_t condition_type = soul_type_checker_traverse(
			    type_checker, node->as.stmt_if.condition);
			if (condition_type != soul_basic_type_bool)
				return soul_basic_type_unknown;
			soul_basic_type_t then_body_type = soul_type_checker_traverse(
			    type_checker, node->as.stmt_if.then_body);
			if (then_body_type == soul_basic_type_unknown)
				return then_body_type;
			if (node->as.stmt_if.else_body)
			{
				soul_basic_type_t else_body_type = soul_type_checker_traverse(
				    type_checker, node->as.stmt_if.else_body);
				if (else_body_type == soul_basic_type_unknown)
					return else_body_type;
			}
		}
		case soul_ast_stmt_for:
		case soul_ast_stmt_while:
		case soul_ast_stmt_block: {
			soul_ast_node_array_t* stmts = &node->as.stmt_block.stmts;
			for (size_t i = 0; i < stmts->size; ++i)
			{
				soul_basic_type_t result
				    = soul_type_checker_traverse(type_checker, stmts->nodes[i]);
				if (result == soul_basic_type_unknown) return result;
			}
		}
		case soul_ast_stmt_return:
		case soul_ast_stmt_variable_decl: {
		}
		case soul_ast_stmt_function_decl:
		case soul_ast_stmt_native_decl:
		default:
			break;
	}
	return soul_basic_type_unknown;
}
soul_type_checker_result_t soul_type_checker_analyze(
    soul_type_checker_t* type_checker, soul_ast_node_t* root)
{
	soul_basic_type_t result_type
	    = soul_type_checker_traverse(type_checker, root);
	if (result_type == soul_basic_type_unknown)
	{
		const char* error_message
		    = "todo: type checking failed. this should return a boolean and "
		      "error message should be a callback instead.";
		return (soul_type_checker_result_t){
		    .status               = soul_type_checker_status_ok,
		    .error_message        = error_message,
		    .error_message_length = strlen(error_message)};
	}

	return (soul_type_checker_result_t){.status = soul_type_checker_status_ok,
	                                    .error_message        = NULL,
	                                    .error_message_length = 0};
}

static void type_checker_register_type(soul_type_checker_t* type_checker) {}

static soul_type_checker_result_t type_checker_error(const char* message)
{
	return (soul_type_checker_result_t){
	    .status               = soul_type_checker_status_error,
	    .error_message        = message,
	    .error_message_length = strlen(message)};
}

//
// TEMP EXPERIMENTATION AREA
//

typedef enum soul_cast_type_t
{
	soul_cast_implicit,
	soul_cast_explicit,
	soul_cast_impossible,
} soul_cast_type_t;

typedef struct soul_cast_t soul_cast_t;
struct soul_cast_t
{
	soul_cast_type_t type;
	soul_basic_type_t from;
	soul_basic_type_t to;
};

#define cast_table_size 5
soul_cast_t cast_table[5] = {
    {soul_cast_implicit, soul_basic_type_integer, soul_basic_type_integer},
    {soul_cast_implicit, soul_basic_type_float, soul_basic_type_float},
    {soul_cast_implicit, soul_basic_type_string, soul_basic_type_string},
    {soul_cast_explicit, soul_basic_type_integer, soul_basic_type_float},
    {soul_cast_explicit, soul_basic_type_float, soul_basic_type_integer},
};

soul_cast_type_t get_cast_type(soul_basic_type_t from, soul_basic_type_t to)
{
	for (size_t i = 0; i < cast_table_size; ++i)
	{
		soul_cast_t* cast = &cast_table[i];
		if (cast->from == from && cast->to == to) return cast->type;
	}

	return soul_cast_impossible;
}

soul_basic_type_t cast_to_common_type(soul_basic_type_t from,
                                      soul_basic_type_t to)
{
	if (get_cast_type(from, to) == soul_cast_impossible)
		return soul_basic_type_unknown;
	return to;
};

soul_basic_type_t type_checker_compare_types(soul_basic_type_t first,
                                             soul_basic_type_t second)
{
	if (first == second)
		return first;
	else if (get_cast_type(first, second) != soul_cast_impossible)
		return cast_to_common_type(first, second);
	return soul_basic_type_unknown;
}
