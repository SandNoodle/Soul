#include "type_checker.h"

#include <assert.h> // TODO: TEMP

typedef enum soul_builtin_type_t : uint8_t
{
	soul_type_void, // Can be only used for functions.
	soul_type_integer,
	soul_type_real,
	soul_type_boolean,
	soul_type_string,
	soul_type_struct,
	soul_type_enum,
} soul_builtin_type_t;

// TODO: Move out to ast for CAST node.
typedef enum soul_cast_type_t
{
	soul_cast_type_implicit,
	soul_cast_type_explicit,
	soul_cast_type_impossible,
} soul_cast_type_t;

typedef struct soul_type_checker_result_t soul_type_checker_result_t;
struct soul_type_checker_result_t
{
	bool is_valid;
	soul_builtin_type_t type;
};

static void type_checker_error(soul_type_checker_t*, const char* format, ...);
static void type_checker_panic(soul_type_checker_t*, const char* format, ...);

// Expressions
static soul_type_checker_result_t check_assign_expression(soul_type_checker_t*, soul_ast_node_t*);
static soul_type_checker_result_t check_unary_expression(soul_type_checker_t*, soul_ast_node_t*);
static soul_type_checker_result_t check_binary_expression(soul_type_checker_t*, soul_ast_node_t*);
static soul_type_checker_result_t check_variable_literal_expression(soul_type_checker_t*, soul_ast_node_t*);
static soul_type_checker_result_t check_boolean_literal_expression(soul_type_checker_t*, soul_ast_node_t*);
static soul_type_checker_result_t check_number_literal_expression(soul_type_checker_t*, soul_ast_node_t*);
static soul_type_checker_result_t check_string_literal_expression(soul_type_checker_t*, soul_ast_node_t*);
static soul_type_checker_result_t check_expression_statement(soul_type_checker_t*, soul_ast_node_t*);

// Statements
static soul_type_checker_result_t check_variable_declaration_statement(soul_type_checker_t*, soul_ast_node_t*);
static soul_type_checker_result_t check_function_declaration_statement(soul_type_checker_t*, soul_ast_node_t*);
static soul_type_checker_result_t check_native_declaration_statement(soul_type_checker_t*, soul_ast_node_t*);
static soul_type_checker_result_t check_if_statement(soul_type_checker_t*, soul_ast_node_t*);
static soul_type_checker_result_t check_for_statement(soul_type_checker_t*, soul_ast_node_t*);
static soul_type_checker_result_t check_while_statement(soul_type_checker_t*, soul_ast_node_t*);
static soul_type_checker_result_t check_block_statement(soul_type_checker_t*, soul_ast_node_t*);
static soul_type_checker_result_t check_return_statement(soul_type_checker_t*, soul_ast_node_t*);

static bool is_type_declaration(soul_ast_node_type_t);

static soul_cast_type_t get_cast_type(soul_builtin_type_t from, soul_builtin_type_t to);

static const char* type_to_string(soul_builtin_type_t type);

soul_type_checker_t soul_type_checker_create(soul_allocator_t* allocator)
{
	soul_type_checker_t checker;
	checker.flags = soul_type_checker_flag_none;
	checker.had_error = false;
	checker.had_panic = false;
	checker.allocator = allocator;
	return checker;
}

soul_type_checker_result_t type_check(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	// TODO: Descent depth-first AST, fail-and-bail on error?, continue on panic.
}

bool soul_type_checker_check(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	if(!type_checker || !node) return false;
	return type_check(type_checker, node).is_valid;
}

static void type_checker_error(soul_type_checker_t* type_checker, const char* format, ...)
{
	type_checker->had_error = true;
	// TODO: Callback
}

static void type_checker_panic(soul_type_checker_t* type_checker, const char* format, ...)
{
	type_checker->had_panic = true;
	// TODO: Callback
}

static soul_type_checker_result_t check_assign_expression(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	soul_type_checker_result_t lhs = type_check(type_checker, node->as.expr_assign.lhs);
	soul_type_checker_result_t rhs = type_check(type_checker, node->as.expr_assign.rhs);

	soul_cast_type_t cast_type = get_cast_type(rhs.type, lhs.type);

	if(cast_type == soul_cast_type_impossible)
	{
		type_checker_error(type_checker,
				"Cast from '%s' to '%s' failed, becasue it is impossible.",
				type_to_string(lhs.type), type_to_string(rhs.type));
		return (soul_type_checker_result_t) {
			.type = lhs.type,
			.is_valid = false,
		};
	}

	if(type_checker->flags & soul_type_checker_flag_strict_casts)
	{
		if(cast_type == soul_cast_type_explicit)
		{
			type_checker_panic(type_checker,
				"Cast from type '%s' to '%s' is required to be explicit - please add a cast.",
				type_to_string(lhs.type), type_to_string(rhs.type));
		}
	}

	return (soul_type_checker_result_t) {
		.type = lhs.type,
		.is_valid = true,
	};
}

static soul_type_checker_result_t check_unary_expression(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	bool is_unary = soul_is_unary_operator(node->as.expr_unary.op);
	soul_type_checker_result_t result = type_check(type_checker, node->as.expr_unary.expr);
	return (soul_type_checker_result_t) {
		.type = result.type,
		.is_valid = is_unary && result.is_valid,
	};
}

static soul_type_checker_result_t check_binary_expression(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	assert(false && "unimplemented");
	// TODO: Type check LHS and RHS and verify if both sides are compatible with each other (castable).
}

static soul_type_checker_result_t check_variable_literal_expression(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	assert(false && "unimplemented");
	// TODO: Check TheStructureTM if there exists variable of a given name.
}

static soul_type_checker_result_t check_boolean_literal_expression(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	return (soul_type_checker_result_t) {
		.type = soul_type_boolean,
		.is_valid = true,
	};
}

static soul_type_checker_result_t check_number_literal_expression(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	return (soul_type_checker_result_t) {
		.type = soul_type_integer, // TODO: tagged union.
		.is_valid = true,
	};
}

static soul_type_checker_result_t check_string_literal_expression(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	return (soul_type_checker_result_t) {
		.type = soul_type_string,
		.is_valid = true,
	};
}

static soul_type_checker_result_t check_expression_statement(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	return type_check(type_checker, node->as.expr_stmt.stmt);
}

static soul_type_checker_result_t check_variable_declaration_statement(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	assert(false && "unimplemented");
	// TODO: Verify if id is not already colliding with something else (ex. other variables, functions, keywords)
	// TODO: Verify if TheStructureTM contains given type - error if not.
	// TODO: type check expression and verify if cast_type != impossible. (Warn in strict mode if explict).
}

static soul_type_checker_result_t check_function_declaration_statement(soul_type_checker_t* type_checker, soul_ast_node_t*)
{
	assert(false && "unimplemented");
	// TODO: Verify if id is not already colliding with something else (ex. other variables, functions, keywords)
	// TODO: If function returns non-void type then Verify if TheStructureTM contains given return type - error if not.
	// TODO: Verify each input param
	// TODO: Verify body.

	// RETURN TYPE: Return node's type for non-void, void otherwise.
}

static soul_type_checker_result_t check_native_declaration_statement(soul_type_checker_t* type_checker, soul_ast_node_t*)
{
	assert(false && "unimplemented");
	// TODO: Verify if id is not already colliding with something else (ex. other variables, functions, keywords)
	// TODO: If function returns non-void type then Verify if TheStructureTM contains given return type - error if not.
	// TODO: Verify each input param
}

static soul_type_checker_result_t check_if_statement(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	assert(false && "unimplemented");
	// TODO: Verify if condition type checks into a boolean expr.
	// TODO: Verify then_body.
	// TODO: (optional) Verify else_body.
}

static soul_type_checker_result_t check_for_statement(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	assert(false && "unimplemented");
	// TODO: (optional) Verify initializer
	// TODO: (optional) Verify if condition type checks into a boolean expr.
	// TODO: (optional) Verify increment_stmt
	// TODO: Verify body.
}

static soul_type_checker_result_t check_while_statement(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	assert(false && "unimplemented");
	// TODO: (optional) Verify if condition type checks into a boolean expr.
	// TODO: Verify body.
}

static soul_type_checker_result_t check_block_statement(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	assert(false && "unimplemented");
	// Type check for each node registering types along the way.
	// IMPORTANT: Discard types after going out of scope.
	// IMPORTANT: DON'T stop type checking block if one statement fails - just enter panic mode,
	//            becase we can catch more potential (useful) errors.
}

static soul_type_checker_result_t check_return_statement(soul_type_checker_t* type_checker, soul_ast_node_t* node)
{
	assert(false && "unimplemented");
	return type_check(type_checker, node->as.stmt_return.expr);
}

static bool is_type_declaration(soul_ast_node_type_t type)
{
	return type == soul_ast_stmt_variable_decl
		|| type == soul_ast_stmt_function_decl
		|| type == soul_ast_stmt_native_decl;
}

static soul_cast_type_t get_cast_type(soul_builtin_type_t from, soul_builtin_type_t to)
{
	if (from == to)
	{
		return !(to == soul_type_struct || to == soul_type_enum)
			? soul_cast_type_implicit : soul_cast_type_impossible;
	}

	if (from == soul_type_integer && to == soul_type_boolean)
		return soul_cast_type_implicit;

	if (from == soul_type_integer && to == soul_type_real)
		return soul_cast_type_implicit;

	if (from == soul_type_real && to == soul_type_integer)
		return soul_cast_type_explicit;

	return soul_cast_type_impossible;
}

static const char* type_to_string(soul_builtin_type_t type)
{
	const char* strings[] = {
		[soul_type_integer] = "integer",
		[soul_type_real]    = "real",
		[soul_type_boolean] = "boolean",
		[soul_type_string]  = "string",
		[soul_type_struct]  = "struct",
		[soul_type_enum]    = "enum",
	};
	return strings[type];
}
