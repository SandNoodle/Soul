#include "compiler.h"

#include "soul_config.h"
#include "ast/ast.h"
#include "runtime/chunk.h"
#include "runtime/opcode.h"

#include <stdlib.h>

// Expressions
static void compiler_compile_unary_expression(soul_compiler_t*, soul_ast_node_t*);
static void compiler_compile_binary_expression(soul_compiler_t*, soul_ast_node_t*);
static void compiler_compile_bool_literal_expression(soul_compiler_t*, soul_ast_node_t*);

// Statements
static void compiler_compile_if_statement(soul_compiler_t*, soul_ast_node_t*);
static void compiler_compile_block_statement(soul_compiler_t*, soul_ast_node_t*);

// Utils
void compiler_enter_scope(soul_compiler_t*);
void compiler_exit_scope(soul_compiler_t*);
uint32_t compiler_emit_opcode(soul_compiler_t*, soul_opcode_t);
void compiler_emit_byte(soul_compiler_t*, uint8_t);
void compiler_emit_short(soul_compiler_t*, uint16_t);
void compiler_patch_short(soul_compiler_t*, uint32_t address, uint16_t value);
uint32_t compiler_get_current_address(soul_compiler_t*);

soul_compiler_t soul_compiler_create(void)
{
	soul_compiler_t compiler;
	return compiler;
}

static void compiler_compile_node(soul_compiler_t* compiler, soul_ast_node_t* node)
{
	// @TODO:
	switch(node->type)
	{
		case soul_ast_expr_assign:
			break;
		case soul_ast_expr_unary:
			return compiler_compile_unary_expression(compiler, node);
		case soul_ast_expr_binary:
			return compiler_compile_binary_expression(compiler, node);
		case soul_ast_expr_var_literal:
			break;
		case soul_ast_expr_bool_literal:
			return compiler_compile_bool_literal_expression(compiler, node);
		case soul_ast_expr_number_literal:
		case soul_ast_expr_string_literal:
		case soul_ast_expr_stmt:
			break;
		case soul_ast_stmt_if:
			return compiler_compile_if_statement(compiler, node);
		case soul_ast_stmt_for:
		case soul_ast_stmt_while:
		case soul_ast_stmt_block:
		case soul_ast_stmt_return:
		case soul_ast_stmt_variable_decl:
		case soul_ast_stmt_function_decl:
		case soul_ast_stmt_native_decl:
		default:
			// @TODO: Propagate error
			break;
	}
}

static void compiler_compile_operand(soul_compiler_t* compiler, soul_ast_node_operator_t op)
{
	switch(op)
	{
		// @TODO:
		default:
			// @TODO: Propagate error
			break;
	}
}

static void compiler_compile_unary_expression(soul_compiler_t* compiler, soul_ast_node_t* node)
{
	compiler_compile_node(compiler, node->as.expr_unary.expr);
	compiler_compile_operand(compiler, node->as.expr_unary.op);
}

static void compiler_compile_binary_expression(soul_compiler_t* compiler, soul_ast_node_t* node)
{
	compiler_compile_node(compiler, node->as.expr_binary.lhs);
	compiler_compile_node(compiler, node->as.expr_binary.rhs);
	compiler_compile_operand(compiler, node->as.expr_binary.op);
}

static void compiler_compile_bool_literal_expression(soul_compiler_t* compiler, soul_ast_node_t* node)
{
	soul_opcode_t value = node->as.expr_literal_bool.val ? soul_op_push_true : soul_op_push_false;
	compiler_emit_opcode(compiler, value);
}

static void compiler_compile_if_statement(soul_compiler_t* compiler, soul_ast_node_t* node)
{
	// Condition
	compiler_compile_node(compiler, node->as.stmt_if.condition);

	// Jumping point
	uint16_t jump_address = compiler_emit_opcode(compiler, soul_op_jump_false);
	compiler_emit_short(compiler, 0x00);

	// Main branch
	compiler_compile_node(compiler, node->as.stmt_if.then_body);

	// Patch the Main branch
	uint16_t current_address = compiler_get_current_address(compiler);
	compiler_patch_short(compiler, jump_address, current_address);

	// (Optional) Else branch
	if(node->as.stmt_if.else_body)
	{
		jump_address = compiler_get_current_address(compiler);
		compiler_compile_node(compiler, node->as.stmt_if.else_body);
		current_address = compiler_get_current_address(compiler);
		compiler_patch_short(compiler, jump_address, current_address);
	}
}

static void compiler_compile_block_statement(soul_compiler_t* compiler, soul_ast_node_t* node)
{
	compiler_enter_scope(compiler);
	soul_ast_node_array_t* statements = &node->as.stmt_block.stmts;
	for(size_t i = 0; i < statements->size; ++i)
	{
		compiler_compile_node(compiler, statements->nodes[i]);
	}
	compiler_exit_scope(compiler);
}

soul_chunk_t soul_compiler_compile(soul_compiler_t* compiler, soul_ast_node_t* root)
{
	if(!compiler || !root) return soul_chunk_create();

	soul_chunk_t chunk = soul_chunk_create();
	compiler->chunk = &chunk;
	compiler_compile_node(compiler, root);

	return chunk;
}

uint32_t compiler_emit_opcode(soul_compiler_t* compiler, soul_opcode_t op)
{
	// @TODO
	return 0;
}

void compiler_emit_byte(soul_compiler_t* compiler, uint8_t b)
{
	// @TODO
}

void compiler_emit_short(soul_compiler_t* compiler, uint16_t s)
{
	// @TODO
}

void compiler_patch_short(soul_compiler_t* compiler, uint32_t address, uint16_t value)
{
	// @TODO
}

uint32_t compiler_get_current_address(soul_compiler_t* compiler)
{
	// @TODO
	return 0;
}

