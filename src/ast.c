#include "ast.h"

#include <stdio.h>

//
// Misc
//

void soul__ast_print_expression(soul_ast_expression_t* e)
{
	if(!e) return;

	switch(e->type)
	{
		case AST_EXPR_ASSIGN:
		case AST_EXPR_BINARY:
		case AST_EXPR_UNARY:
		case AST_EXPR_BOOL_LITERAL:
		case AST_EXPR_NUMBER_LITERAL:
		case AST_EXPR_STRING_LITERAL:
			break;
	}
}

void soul__ast_print_statement(soul_ast_statement_t* s)
{
	if(!s) return;

	switch(s->type)
	{
		case AST_STMT_IF:
		case AST_STMT_FOR:
		case AST_STMT_FOREACH:
		case AST_STMT_WHILE:
		case AST_STMT_BLOCK:
		case AST_STMT_RETURN:
		case AST_STMT_IMPORT:
			break;
		case AST_STMT_VARIABLE_DECL:
			soul_ast_identifier_t* id = s->as.decl_stmt.var_decl.id;
			int64_t val = s->as.decl_stmt.var_decl.val->as.number_literal_expr.val.as.i64;
			printf("[VAR_DECL, '%.*s' = %lld]\n", (int)id->length, id->name, val);
		case AST_STMT_FUNCTION_DECL:
		case AST_STMT_NATIVE_DECL:
		case AST_STMT_DEFINE_DECL:
			break;
	}
}

void soul__ast_print(soul_ast_t ast)
{
	if(!ast.valid)
	{
		const char* message = "Cannot print an AST, because it is invalid!";
		printf("%s\n", message);
		return;
	}

	soul__ast_print_statement(ast.root);
}

//
// Identifiers
//

// @TODO REMOVE MALLOC FOR USER DEFINED ALLOCATOR
soul_ast_identifier_t* soul__ast_new_identifier(const char* name, size_t length)
{
	soul_ast_identifier_t* i = (soul_ast_identifier_t*)malloc(sizeof(soul_ast_identifier_t));
	i->length = length;
	i->name = name;
	return i;
}

// @TODO REMOVE FREE FOR USER DEFINED DEALLOCATOR
void soul__ast_free_identifier(soul_ast_identifier_t* i)
{
	i->name = NULL;
	i->length = 0;
	free(i);
}

static bool soul__ast_are_identifiers_equal(soul_ast_identifier_t* l, soul_ast_identifier_t* r)
{
	return (l->length == r->length)                    // Same size
		&& (memcmp(l->name, r->name, l->length)) == 0; // Same string
}

//
// Expressions
//

// @TODO REMOVE MALLOC FOR USER DEFINED ALLOCATOR
soul_ast_expression_t* soul__ast_new_expression(soul_ast_expression_type_t type,
	uint32_t line)
{
	soul_ast_expression_t* expr = (soul_ast_expression_t*)malloc(sizeof(soul_ast_expression_t));
	expr->line = line;
	expr->type = type;
	return expr;
}

soul_ast_expression_t* soul__ast_assgin_expression(soul_ast_expression_t* lval,
	soul_ast_expression_t* rval, uint32_t line)
{
	soul_ast_expression_t* e = soul__ast_new_expression(AST_EXPR_ASSIGN, line);
	e->as.assign_expr.lval = lval;
	e->as.assign_expr.lval = rval;
	return e;
}

soul_ast_expression_t* soul__ast_binary_expression(soul_ast_expression_t* left,
	soul_ast_expression_t* right, soul_token_type_t op, uint32_t line)
{
	soul_ast_expression_t* e = soul__ast_new_expression(AST_EXPR_BINARY, line);
	e->as.binary_expr.left = left;
	e->as.binary_expr.right = right;
	e->as.binary_expr.op = op;
	return e;
}

soul_ast_expression_t* soul__ast_unary_expression(soul_ast_expression_t* expr,
	soul_token_type_t op, uint32_t line)
{
	soul_ast_expression_t* e = soul__ast_new_expression(AST_EXPR_BINARY, line);
	e->as.unary_expr.expr = expr;
	e->as.unary_expr.op = op;
	return e;
}

soul_ast_expression_t* soul__ast_number_literal_expression(soul_value_type_t type,
	soul_value_t value, uint32_t line)
{
	soul_ast_expression_t* e = soul__ast_new_expression(AST_EXPR_NUMBER_LITERAL, line);
	e->as.number_literal_expr.type = type;
	e->as.number_literal_expr.val = value;

	return e;
}

// @TODO REMOVE MALLOC FOR USER DEFINED DEALLOCATOR
void soul__ast_free_expression(soul_ast_expression_t* expression)
{
	if(!expression) return;

	switch(expression->type)
	{
		case AST_EXPR_ASSIGN:
			soul__ast_free_expression(expression->as.assign_expr.lval);
			soul__ast_free_expression(expression->as.assign_expr.rval);
			break;
		case AST_EXPR_BINARY:
			soul__ast_free_expression(expression->as.binary_expr.left);
			soul__ast_free_expression(expression->as.binary_expr.right);
			break;
		case AST_EXPR_UNARY:
			soul__ast_free_expression(expression->as.unary_expr.expr);
			break;
		case AST_EXPR_BOOL_LITERAL:
			break;
		case AST_EXPR_NUMBER_LITERAL:
			break;
		case AST_EXPR_STRING_LITERAL:
			break;
	}

	free(expression);
}

//
// Statements
//
//
// @TODO REMOVE MALLOC FOR USER DEFINED ALLOCATOR
soul_ast_statement_t* soul__ast_new_statement(soul_ast_statement_type_t type, uint32_t line)
{
	soul_ast_statement_t* s = (soul_ast_statement_t*)malloc(sizeof(soul_ast_statement_t));
	s->line = line;
	s->type = type;
	return s;
}

soul_ast_statement_t* soul__ast_new_declaration(soul_ast_statement_type_t type, uint32_t line)
{
	assert( type == AST_STMT_VARIABLE_DECL || type == AST_STMT_FUNCTION_DECL ||
		type == AST_STMT_DEFINE_DECL || type == AST_STMT_NATIVE_DECL);
	return soul__ast_new_statement(type, line);
}

soul_ast_statement_t* soul__ast_variable_declaration(soul_ast_identifier_t* id,
	soul_ast_expression_t* init, uint32_t line)
{
	soul_ast_statement_t* d = soul__ast_new_declaration(AST_STMT_VARIABLE_DECL, line);
	d->as.decl_stmt.var_decl.id = id;
	d->as.decl_stmt.var_decl.val = init;

	return d;
}

soul_ast_statement_t* soul__ast_function_declaration(soul_token_t* name,
	/* @TODO args? */
	soul_ast_statement_t* body,
	uint32_t line)
{
	soul_ast_statement_t* d = soul__ast_new_declaration(AST_STMT_FUNCTION_DECL, line);
	d->as.decl_stmt.fun_decl.id->name = name->start;
	d->as.decl_stmt.fun_decl.id->length = name->length;
	d->as.decl_stmt.fun_decl.body = body;
	return d;
}

soul_ast_statement_t* soul__ast_if_statement(soul_ast_expression_t* condition,
	soul_ast_statement_t* then_stmt, soul_ast_statement_t * else_stmt, uint32_t line)
{
	soul_ast_statement_t* s = soul__ast_new_statement(AST_STMT_IF, line);
	s->as.if_stmt.condition = condition;
	s->as.if_stmt.then_stmt = then_stmt;
	s->as.if_stmt.else_stmt = else_stmt;
	return s;
}

soul_ast_statement_t* soul__ast_for_statement( soul_ast_statement_t* var,
	soul_ast_expression_t* condition, soul_ast_expression_t* actual,
	soul_ast_statement_t* body, uint32_t line)
{
	soul_ast_statement_t* s = soul__ast_new_statement(AST_STMT_FOR, line);
	s->as.for_stmt.var = var;
	s->as.for_stmt.condition = condition;
	s->as.for_stmt.actual = actual;
	s->as.for_stmt.body = body;

	return s;
}

soul_ast_statement_t* soul__ast_while_statement(soul_ast_expression_t* condition,
	soul_ast_statement_t* body, uint32_t line)
{
	soul_ast_statement_t* s = soul__ast_new_statement(AST_STMT_WHILE, line);
	s->as.while_stmt.condition = condition;
	s->as.while_stmt.body = body;

	return s;
}

// @TODO REMOVE MALLOC FOR USER DEFINED DEALLOCATOR
// @TODO !!!!
void soul__ast_free_statement(soul_ast_statement_t* s)
{
	if(!s) return;

	switch(s->type)
	{
		case AST_STMT_IF:
			soul__ast_free_expression(s->as.if_stmt.condition);
			soul__ast_free_statement(s->as.if_stmt.then_stmt);
			soul__ast_free_statement(s->as.if_stmt.else_stmt);
			break;
		case AST_STMT_FOR:
			break;
		case AST_STMT_FOREACH:
			break;
		case AST_STMT_WHILE:
			soul__ast_free_expression(s->as.while_stmt.condition);
			soul__ast_free_statement(s->as.while_stmt.body);
			break;
		case AST_STMT_BLOCK:
			break;
		case AST_STMT_RETURN:
			soul__ast_free_expression(s->as.return_stmt.return_expression);
			break;
		case AST_STMT_IMPORT:
			break;
		case AST_STMT_VARIABLE_DECL:
			soul__ast_free_identifier(s->as.decl_stmt.var_decl.id);
			soul__ast_free_expression(s->as.decl_stmt.var_decl.val);
			break;
		case AST_STMT_FUNCTION_DECL:
			soul__ast_free_identifier(s->as.decl_stmt.fun_decl.id);
			soul__ast_free_statement(s->as.decl_stmt.fun_decl.body);
			break;
		case AST_STMT_NATIVE_DECL:
			soul__ast_free_identifier(s->as.decl_stmt.native_decl.id);
			break;
		case AST_STMT_DEFINE_DECL:
			soul__ast_free_identifier(s->as.decl_stmt.define_decl.id);
			break;
	}

	free(s);
}

SOUL_API void soul_ast_free(soul_ast_t* ast)
{
	// @TEMP SUPRESS WARNINGS IN THIS FILE
	SOUL_UNUSED(soul__ast_are_identifiers_equal);
	//

	if(!ast->valid || !ast->root) return;
	soul__ast_free_statement(ast->root);
}