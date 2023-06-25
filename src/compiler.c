#include "compiler.h"

#include "opcode.h"
#include "vm.h"

#include <stdio.h> // @TEMP
#include <stdarg.h>

struct soul_variable_t {
	soul_ast_identifier_t* id;
	int32_t depth;
};

SOUL_VECTOR_DECLARE(chunk_constants, soul_value_t)
SOUL_VECTOR_DECLARE(chunk_data, uint8_t)
SOUL_STACK_DECLARE(variable, soul_variable_t)

static void soul__compiler_compile_statement(soul_compiler_t* c, soul_ast_statement_t* s);
static void soul__compiler_compile_expression(soul_compiler_t* c, soul_ast_expression_t* e);

static void soul__compiler_init(soul_compiler_t* compiler, soul_chunk_t* chunk)
{
	compiler->had_panic = false;
	compiler->had_error = false;
	compiler->current_chunk = chunk;
	compiler->current_depth = 0;

	soul__variable_stack_new(&compiler->locals);
}

static void soul__compiler_free(soul_compiler_t* compiler)
{
	soul__variable_stack_free(&compiler->locals);
}

static void soul__compiler_error(soul_compiler_t* compiler, const char* message, ...)
{
	if(compiler->had_panic) return;
	compiler->had_panic = true;
	compiler->had_error = true;

#if 0
	if(parser->error_callback)
	{
		const soul_token_t token = parser->current_token;
		compiler->error_callback(NULL, token.line, message, strlen(message));
	}
#else
	// @TEMP @nocheckin
	va_list varg;
	va_start(varg, message);
	printf("[ERROR] PARSER: ");
	vprintf(message, varg);
	printf("\n");
	va_end(varg);
#endif
}

static void soul__compiler_chunk_init(soul_chunk_t* c)
{
	soul__chunk_data_vector_new(&c->code);
	soul__chunk_constants_vector_new(&c->constants);
	c->valid = true;
}

static size_t soul__compiler_chunk_write_byte(soul_chunk_t* chunk, uint8_t byte)
{
	soul__chunk_data_vector_push(&chunk->code, byte);
	return chunk->code.size;
}

static uint32_t soul__compiler_chunk_add_constant(soul_chunk_t* chunk, soul_value_t value)
{
	size_t index = chunk->constants.size;
	soul__chunk_constants_vector_push(&chunk->constants, value);
	return index;
}

static void soul__compiler_emit_opcode(soul_chunk_t* chunk, opcode_t op)
{
	soul__compiler_chunk_write_byte(chunk, op);
}

static void soul__compiler_emit_byte(soul_chunk_t* chunk, uint8_t b)
{
	soul__compiler_chunk_write_byte(chunk, b);
}

static void soul__compiler_emit_short(soul_chunk_t* chunk, uint16_t s)
{
	soul__compiler_chunk_write_byte(chunk, (s >> 8) & 0xff);
	soul__compiler_chunk_write_byte(chunk, (s >> 0) & 0xff);
}

// Discard all scopes higher than a current scope.
static size_t soul__compiler_discard_scopes(soul_compiler_t* c)
{
	size_t to_discard = 0;
	for(size_t index = c->locals.size - 1; index < c->locals.size; --index)
	{
		if(c->locals.data[index].depth == c->current_depth)
		{
			to_discard = c->locals.size - index + 1;
			break;
		}
	}

	if(to_discard > 1)
	{
		soul__compiler_emit_opcode(c->current_chunk, OP_POP);
		soul__compiler_emit_byte(c->current_chunk, to_discard);
	}

	return to_discard;
}

static void soul__compiler_enter_scope(soul_compiler_t* c)
{
	c->current_depth++;
}

static void soul__compiler_exit_scope(soul_compiler_t* c)
{
	c->current_depth--;
	size_t to_discard = soul__compiler_discard_scopes(c);
	soul__variable_stack_popn(&c->locals, to_discard);
}

//
// Expressions
//

static void soul__compiler_compile_assign_expression(soul_compiler_t* c, soul_ast_expression_t* e)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(e);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_binary_expression(soul_compiler_t* c, soul_ast_expression_t* e)
{
	soul__compiler_compile_expression(c, e->as.binary_expr.rval);
	soul__compiler_compile_expression(c, e->as.binary_expr.lval);
	switch(e->as.binary_expr.op)
	{
		case TOKEN_PLUS:
			soul__compiler_emit_opcode(c->current_chunk, OP_ADDI); // @TODO OP based on type.
			break;
		case TOKEN_MINUS:
			soul__compiler_emit_opcode(c->current_chunk, OP_SUBI); // @TODO OP based on type.
			break;
		case TOKEN_STAR:
			soul__compiler_emit_opcode(c->current_chunk, OP_MULI); // @TODO OP based on type.
			break;
		case TOKEN_SLASH:
			soul__compiler_emit_opcode(c->current_chunk, OP_DIVI); // @TODO OP based on type.
			break;
		default:
			SOUL_UNIMPLEMENTED();
			break;
	}
}

static void soul__compiler_compile_unary_expression(soul_compiler_t* c, soul_ast_expression_t* e)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(e);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_bool_literal_expression(soul_compiler_t* c, soul_ast_expression_t* e)
{
	soul__compiler_emit_opcode(c->current_chunk, OP_GET_CONST);
	soul__compiler_emit_byte(c->current_chunk, e->as.bool_literal_expr.val);
}

static void soul__compiler_compile_number_literal_expression(soul_compiler_t* c, soul_ast_expression_t* e)
{
	uint32_t index = soul__compiler_chunk_add_constant(c->current_chunk, e->as.number_literal_expr.val);
	soul__compiler_emit_opcode(c->current_chunk, OP_GET_CONST);
	soul__compiler_emit_byte(c->current_chunk, index); // @TODO index is 32 bit, but we accept only 8 bit at max!
}

static void soul__compiler_compile_string_literal_expression(soul_compiler_t* c, soul_ast_expression_t* e)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(e);
	SOUL_UNIMPLEMENTED();
}

static int32_t soul__compiler_resolve_variable(soul_compiler_t* c, soul_ast_identifier_t* id)
{
	for(size_t index = c->locals.size - 1; index < c->locals.size; --index)
	{
		if(soul__ast_are_identifiers_equal(c->locals.data[index].id, id))
			return index;
	}

	return -1;
}
static void soul__compiler_compile_variable_literal_expression(soul_compiler_t* c, soul_ast_expression_t* e)
{
	soul_ast_identifier_t* id = e->as.var_literal_expr.id;
	int32_t variable_index = soul__compiler_resolve_variable(c, id);
	if(variable_index == -1)
		soul__compiler_error(c, "Variable '%.*s' was not defined in this scope!",
			(int)id->length, id->name);

	soul__compiler_emit_opcode(c->current_chunk, OP_GET_LOCAL);
	soul__compiler_emit_byte(c->current_chunk, variable_index);
}


static void soul__compiler_compile_expression(soul_compiler_t* c,
	soul_ast_expression_t* e)
{
	switch(e->type)
	{
		case AST_EXPR_ASSIGN:         soul__compiler_compile_assign_expression(c, e); break;
		case AST_EXPR_BINARY:         soul__compiler_compile_binary_expression(c, e); break;
		case AST_EXPR_UNARY:          soul__compiler_compile_unary_expression(c, e); break;
		case AST_EXPR_BOOL_LITERAL:   soul__compiler_compile_bool_literal_expression(c, e); break;
		case AST_EXPR_NUMBER_LITERAL: soul__compiler_compile_number_literal_expression(c, e); break;
		case AST_EXPR_STRING_LITERAL: soul__compiler_compile_string_literal_expression(c, e); break;
		case AST_EXPR_VAR_LITERAL:    soul__compiler_compile_variable_literal_expression(c, e); break;
	}
}

//
// Statements
//

static void soul__compiler_compile_if_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_for_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_foreach_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_while_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_block_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	soul__compiler_enter_scope(c);
	soul_ast_statement_vector_t* stmts = s->as.block_stmt.stmts;
	for(size_t i = 0; i < stmts->size; ++i)
	{
		soul__compiler_compile_statement(c, stmts->data[i]);
	}
	soul__compiler_exit_scope(c);
}

static void soul__compiler_compile_return_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_import_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static int32_t soul__compiler_add_local_variable(soul_compiler_t* c, soul_ast_identifier_t* id)
{
	soul_variable_t var;
	var.id = id;
	var.depth = c->current_depth;
	soul__variable_stack_push(&c->locals, var);
	return c->locals.size - 1; // Return index.
}

static void soul__compiler_compile_variable_declaration_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	if(c->current_depth < 1)
	{
		soul__compiler_error(c, "Declaration of global variables is NOT supported!");
		return;
	}

	if(c->locals.size >= SOUL_MAX_LOCAL_VARIABLES)
	{
		// @TODO Current function name.
		soul__compiler_error(c, "Function '%s' has too many local variables (Limit %d)!",
			"@TODO", SOUL_MAX_LOCAL_VARIABLES);
		return;
	}

	soul_ast_identifier_t* id = s->as.decl_stmt.var_decl.id;

	// @TODO DISALLOW SHADOWING OF VARIABLES REGARDLESS OF SCOPE

	// Declare variable.
	int32_t variable_index = soul__compiler_add_local_variable(c, id);

	// NOTE: Soul requires all of its variables to be defined as they are
	//       declared. So we can compile its r-value expression just before
	//       we emit OP_SET_LOCAL - no further steps required!.
	soul__compiler_compile_expression(c, s->as.decl_stmt.var_decl.val);

	// Declare variable.
	soul__compiler_emit_opcode(c->current_chunk, OP_SET_LOCAL);
	soul__compiler_emit_byte(c->current_chunk, variable_index);
}

static void soul__compiler_compile_function_declaration_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_native_declaration_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_define_declaration_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_expression_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_statement(soul_compiler_t* c,
	soul_ast_statement_t* s)
{
	switch(s->type)
	{
		case AST_STMT_IF:              soul__compiler_compile_if_statement(c, s); break;
		case AST_STMT_FOR:             soul__compiler_compile_for_statement(c, s); break;
		case AST_STMT_FOREACH:         soul__compiler_compile_foreach_statement(c, s); break;
		case AST_STMT_WHILE:           soul__compiler_compile_while_statement(c, s); break;
		case AST_STMT_BLOCK:           soul__compiler_compile_block_statement(c, s); break;
		case AST_STMT_RETURN:          soul__compiler_compile_return_statement(c, s); break;
		case AST_STMT_IMPORT:          soul__compiler_compile_import_statement(c, s); break;
		case AST_STMT_VARIABLE_DECL:   soul__compiler_compile_variable_declaration_statement(c, s); break;
		case AST_STMT_FUNCTION_DECL:   soul__compiler_compile_function_declaration_statement(c, s); break;
		case AST_STMT_NATIVE_DECL:     soul__compiler_compile_native_declaration_statement(c, s); break;
		case AST_STMT_DEFINE_DECL:     soul__compiler_compile_define_declaration_statement(c, s); break;
		case AST_STMT_EXPRESSION:      soul__compiler_compile_expression_statement(c, s);
	}
}

void soul__disassemble_instruction(soul_chunk_t* c, size_t* index)
{
	printf("%04llu: ", *index);
	const char* op = opcode_to_string(c->code.data[*index]);
	switch(c->code.data[*index])
	{
		case OP_GET_CONST:
		case OP_GET_LOCAL:
		case OP_SET_LOCAL:
			*index += 1;
			printf("%s %d\n", op, c->code.data[*index]);
			break;
		default:
			printf("%s\n", op);
			break;
	}
}

//
// Public API
//

void soul__disassemble_chunk(soul_chunk_t* c)
{
	printf("=== CHUNK ===\n");
	printf("* CODE:\n");
	for(size_t i = 0; i < c->code.size; ++i)
	{
		soul__disassemble_instruction(c, &i);
	}

	printf("* CONSTANTS:\n");
	for(size_t i = 0; i < c->constants.size; ++i)
	{
		printf("%04lld: ", i);
		soul_value_t v = c->constants.data[i];
		switch(v.type)
		{
			case SOUL_TYPE_BOOL:
				printf("%s\n", v.as.type_bool ? "true" : "false");
				break;
			case SOUL_TYPE_INT:
				printf("%lld\n", v.as.type_int);
				break;
			case SOUL_TYPE_REAL:
				printf("%f\n", v.as.type_real);
				break;
			default:
				printf("[UNKNOWN TYPE]");
				break;
		}
	}
	printf("=== ----- ===\n");
}

SOUL_API soul_chunk_t* soul_compile(soul_ast_t* ast)
{
	// @TEMP Supress warning for unused functions
	SOUL_UNUSED(soul__compiler_emit_short);
	SOUL_UNUSED(soul__compiler_error);
	//

	// Prepare chunk
	soul_chunk_t* chunk = (soul_chunk_t*)malloc(sizeof(soul_chunk_t));
	soul__compiler_chunk_init(chunk);

	// Prepare compiler
	soul_compiler_t compiler;
	soul__compiler_init(&compiler, chunk);

	soul__compiler_compile_statement(&compiler, ast->root);

	// @TODO @TEMP Emit print for debugging.
	soul__compiler_emit_opcode(chunk, OP_PRINT);

	soul__disassemble_chunk(chunk);

	soul__compiler_free(&compiler);

	return chunk;
}
