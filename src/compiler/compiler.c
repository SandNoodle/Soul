#include "compiler.h"

#include "ast/ast.h"
#include "runtime/chunk.h"
#include "runtime/opcode.h"
#include "soul_config.h"

#include <stdlib.h>
#include <stdio.h>
#include <assert.h>

// Expressions
static void compile_unary_expression(soul_compiler_t*, soul_ast_node_t*);
static void compile_binary_expression(soul_compiler_t*, soul_ast_node_t*);
static void compile_bool_literal_expression(soul_compiler_t*, soul_ast_node_t*);
static void compile_number_literal_expression(soul_compiler_t*, soul_ast_node_t*);
static void compile_string_literal_expression(soul_compiler_t*, soul_ast_node_t*);

// Statements
static void compile_varaiable_declaration_statement(soul_compiler_t*, soul_ast_node_t*);
static void compile_if_statement(soul_compiler_t*, soul_ast_node_t*);
static void compile_block_statement(soul_compiler_t*, soul_ast_node_t*);

// Utils
void enter_scope(soul_compiler_t*);
void exit_scope(soul_compiler_t*);
bool is_global_scope(soul_compiler_t*);
uint32_t emit_opcode(soul_compiler_t*, soul_opcode_t);
void emit_byte(soul_compiler_t*, uint8_t);
void emit_short(soul_compiler_t*, uint16_t);
void patch_short(soul_compiler_t*, uint32_t address, uint16_t value);
uint32_t get_current_address(soul_compiler_t*);

soul_compiler_t soul_compiler_create(soul_allocator_t* allocator)
{
	soul_compiler_t compiler;
	compiler.chunk = NULL;
	compiler.current_depth = 0;
	compiler.had_error = false;
	compiler.had_panic = false;
	compiler.allocator = allocator;
	return compiler;
}

static void compile_node(soul_compiler_t* compiler,
                         soul_ast_node_t* node)
{
	if(!node) return;

	// @TODO:
	switch (node->type)
	{
		case soul_ast_expr_assign:
			break;
		case soul_ast_expr_unary:
			return compile_unary_expression(compiler, node);
		case soul_ast_expr_binary:
			return compile_binary_expression(compiler, node);
		case soul_ast_expr_var_literal:
			break;
		case soul_ast_expr_bool_literal:
			return compile_bool_literal_expression(compiler, node);
		case soul_ast_expr_number_literal:
			return compile_number_literal_expression(compiler, node);
		case soul_ast_expr_string_literal:
			return compile_string_literal_expression(compiler, node);
		case soul_ast_expr_stmt:
			break;
		case soul_ast_stmt_if:
			return compile_if_statement(compiler, node);
		case soul_ast_stmt_for:
		case soul_ast_stmt_while:
		case soul_ast_stmt_block:
			return compile_block_statement(compiler, node);
		case soul_ast_stmt_return:
		case soul_ast_stmt_variable_decl:
			return compile_varaiable_declaration_statement(compiler, node);
		case soul_ast_stmt_function_decl:
		case soul_ast_stmt_native_decl:
		default:
			// @TODO: Propagate error
			break;
	}
}

static void compile_operand(soul_compiler_t* compiler, soul_ast_node_operator_t op)
{
	switch (op)
	{
		// @TODO:
		case soul_ast_op_add:
			emit_opcode(compiler, soul_op_addi); // TODO: Types
			break;
		case soul_ast_op_sub:
			emit_opcode(compiler, soul_op_subi); // TODO: Types
			break;
		case soul_ast_op_mul:
			emit_opcode(compiler, soul_op_muli); // TODO: Types
			break;
		case soul_ast_op_div:
			emit_opcode(compiler, soul_op_divi); // TODO: Types
			break;
		default:
			// @TODO: Propagate error
			break;
	}
}

static void compile_unary_expression(soul_compiler_t* compiler,
                                              soul_ast_node_t* node)
{
	struct soul_ast_expr_unary_t* e = &node->as.expr_unary;
	compile_node(compiler, e->expr);
	compile_operand(compiler, e->op);
}

static void compile_binary_expression(soul_compiler_t* compiler,
                                               soul_ast_node_t* node)
{
	struct soul_ast_expr_binary_t* e = &node->as.expr_binary;
	compile_node(compiler, e->lhs);
	compile_node(compiler, e->rhs);
	compile_operand(compiler, e->op);
}

static void compile_bool_literal_expression(soul_compiler_t* compiler,
                                                     soul_ast_node_t* node)
{
	soul_opcode_t value = node->as.expr_literal_bool.val ? soul_op_push_true
	                                                     : soul_op_push_false;
	emit_opcode(compiler, value);
}

static void compile_number_literal_expression(soul_compiler_t* compiler, soul_ast_node_t* node)
{
	assert(false && "unimplemented");
}

static void compile_string_literal_expression(soul_compiler_t* compiler, soul_ast_node_t* node)
{
	assert(false && "unimplemented");
}

static void compile_varaiable_declaration_statement(soul_compiler_t* compiler, soul_ast_node_t* node)
{
	if(is_global_scope(compiler))
	{
		// TODO: Error callback.
		return;
	}

	struct soul_ast_stmt_variable_decl_t* s = &node->as.stmt_variable_decl;
	assert(false && "unimplemented");
}

static void compile_if_statement(soul_compiler_t* compiler,
                                          soul_ast_node_t* node)
{
	struct soul_ast_stmt_if_t* s = &node->as.stmt_if;
	// Condition
	compile_node(compiler, s->condition);

	// Jumping point
	uint16_t jump_address = emit_opcode(compiler, soul_op_jump_false);
	emit_short(compiler, 0x00);

	// Main branch
	compile_node(compiler, s->then_body);

	// Patch the Main branch
	uint16_t current_address = get_current_address(compiler);
	patch_short(compiler, jump_address, current_address);

	// (Optional) Else branch
	if (s->else_body)
	{
		jump_address = get_current_address(compiler);
		compile_node(compiler, s->else_body);
		current_address = get_current_address(compiler);
		patch_short(compiler, jump_address, current_address);
	}
}

static void compile_block_statement(soul_compiler_t* compiler,
                                             soul_ast_node_t* node)
{
	enter_scope(compiler);
	soul_ast_node_array_t* statements = &node->as.stmt_block.stmts;
	for (size_t i = 0; i < statements->size; ++i)
	{
		compile_node(compiler, statements->nodes[i]);
	}
	exit_scope(compiler);
}

soul_chunk_t soul_compiler_compile(soul_compiler_t* compiler,
                                   soul_ast_node_t* root)
{
	if (!compiler || !root) return soul_chunk_create(NULL);

	soul_chunk_t chunk = soul_chunk_create(compiler->allocator);
	compiler->chunk    = &chunk;
	compile_node(compiler, root);

	return chunk;
}

void enter_scope(soul_compiler_t* compiler)
{
	compiler->current_depth++;
}

void exit_scope(soul_compiler_t* compiler)
{
	assert(false && "unimplemented");
	/* discard_variables(compiler->, compiler->current_depth); */ // TODO
	compiler->current_depth--;
}

bool is_global_scope(soul_compiler_t* compiler)
{
	return compiler->current_depth == 0;
}

uint32_t emit_opcode(soul_compiler_t* compiler, soul_opcode_t op)
{
	assert(false && "unimplemented");
}

void emit_byte(soul_compiler_t* compiler, uint8_t b)
{
	assert(false && "unimplemented");
}

void emit_short(soul_compiler_t* compiler, uint16_t s)
{
	assert(false && "unimplemented");
}

void patch_short(soul_compiler_t* compiler, uint32_t address,
                          uint16_t value)
{
	assert(false && "unimplemented");
}

uint32_t get_current_address(soul_compiler_t* compiler)
{
	assert(false && "unimplemented");
	return 0;
}
