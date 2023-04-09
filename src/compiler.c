#include "compiler.h"

#include "opcode.h"

static const soul_chunk_t soul__invalid_chunk = {
	.valid = false,
};

static void soul__compiler_init(soul_compiler_t* compiler)
{
	compiler->had_panic = false;
	compiler->had_error = false;
}

static void soul__compiler_error(soul_compiler_t* compiler, const char* message)
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
	(void)(message);
#endif
}

static size_t soul__compiler_chunk_write_byte(soul_chunk_t* chunk, uint8_t byte)
{
	if(chunk->count == chunk->capacity - 1)
	{
		uint32_t new_capacity = chunk->capacity * 2.0f;
		chunk->code = (uint8_t*)realloc(chunk->code, new_capacity);
		chunk->capacity = new_capacity;
	}

	chunk->code[chunk->count] = byte;
	return chunk->count++;
}

static uint32_t soul__compiler_chunk_add_constant(soul_chunk_t* chunk, soul_value_t value)
{
	soul_chunk_constants_t* c = &chunk->constants;
	if(c->count == c->capacity - 1) // @TODO Dynamic array support
	{
		uint32_t new_capacity = c->capacity * 2.0f;
		c->values = (soul_value_t*)realloc(c->values, new_capacity);
		c->capacity = new_capacity;
	}

	size_t index = c->count;
	c->values[index] = value;
	c->count++;
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
	SOUL_UNUSED(c);
	SOUL_UNUSED(e);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_unary_expression(soul_compiler_t* c, soul_ast_expression_t* e)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(e);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_bool_literal_expression(soul_compiler_t* c, soul_ast_expression_t* e)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(e);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_number_literal_expression(soul_compiler_t* c, soul_ast_expression_t* e)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(e);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler_compile_string_literal_expression(soul_compiler_t* c, soul_ast_expression_t* e)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(e);
	SOUL_UNIMPLEMENTED();
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
	}
}

//
// Statements
//

static void soul__compiler__compile_if_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler__compile_for_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler__compile_foreach_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler__compile_while_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler__compile_block_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler__compile_return_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler__compile_import_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler__compile_variable_declaration_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler__compile_function_declaration_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler__compile_native_declaration_statement(soul_compiler_t* c, soul_ast_statement_t* s)
{
	SOUL_UNUSED(c);
	SOUL_UNUSED(s);
	SOUL_UNIMPLEMENTED();
}

static void soul__compiler__compile_define_declaration_statement(soul_compiler_t* c, soul_ast_statement_t* s)
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
		case AST_STMT_IF:              soul__compiler__compile_if_statement(c, s); break;
		case AST_STMT_FOR:             soul__compiler__compile_for_statement(c, s); break;
		case AST_STMT_FOREACH:         soul__compiler__compile_foreach_statement(c, s); break;
		case AST_STMT_WHILE:           soul__compiler__compile_while_statement(c, s); break;
		case AST_STMT_BLOCK:           soul__compiler__compile_block_statement(c, s); break;
		case AST_STMT_RETURN:          soul__compiler__compile_return_statement(c, s); break;
		case AST_STMT_IMPORT:          soul__compiler__compile_import_statement(c, s); break;
		case AST_STMT_VARIABLE_DECL:   soul__compiler__compile_variable_declaration_statement(c, s); break;
		case AST_STMT_FUNCTION_DECL:   soul__compiler__compile_function_declaration_statement(c, s); break;
		case AST_STMT_NATIVE_DECL:     soul__compiler__compile_native_declaration_statement(c, s); break;
		case AST_STMT_DEFINE_DECL:     soul__compiler__compile_define_declaration_statement(c, s); break;
	}
}

//
// Public API
//

SOUL_API soul_chunk_t soul_compile(soul_ast_t ast)
{
	// @TEMP SUPRESS WARNINGS IN THIS FILE
	SOUL_UNUSED(soul__compiler_error);
	SOUL_UNUSED(soul__compiler_chunk_add_constant);
	SOUL_UNUSED(soul__compiler_emit_opcode);
	SOUL_UNUSED(soul__compiler_emit_byte);
	SOUL_UNUSED(soul__compiler_emit_short);
	SOUL_UNUSED(soul__compiler_compile_statement);
	SOUL_UNUSED(soul__compiler_compile_expression);
	//

	if(!ast.valid) return soul__invalid_chunk;

	// Prepare compiler
	soul_compiler_t compiler;
	soul__compiler_init(&compiler);

	// @TODO @nocheckin NOT IMPLEMENTED.
	return soul__invalid_chunk;
}
