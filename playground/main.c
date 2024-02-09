#include "lexer/lexer.h"
#include "lexer/token.h"
#include "parser/parser.h"
#include "ast/ast.h"
#include "ast/ast_stringify.h"
#include "compiler/compiler.h"
#include "runtime/chunk.h"
#include "runtime/opcode.h"
#include "runtime/value.h"
#include "runtime/vm.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SOUL_PRINT_ARRAY
#define SOUL_PRINT_AST
#define SOUL_PRINT_BYTECODE

int main(void)
{
	soul_value_t const_values[] = {
		(soul_value_t){.int_value = 54},
		(soul_value_t){.int_value = 45},
		(soul_value_t){.int_value = 12},
		(soul_value_t){.int_value = 8},
		(soul_value_t){.int_value = 4},
		(soul_value_t){.int_value = 1},
	};
	soul_value_array_t constants = soul_value_array_create();
	const size_t array_size = sizeof(const_values) / sizeof(const_values[0]);
	for(size_t i = 0; i < array_size; ++i)
	{
		soul_value_array_append(&constants, const_values[i]);
	}
	uint8_t code[] = {
		soul_op_push_const, 0x00,
		soul_op_push_const, 0x01,
		soul_op_addi,
		soul_op_push_const, 0x05,
		soul_op_addi,
		soul_op_push_const, 0x02,
		soul_op_muli,
		soul_op_push_const, 0x03,
		soul_op_push_const, 0x04,
		soul_op_divi,
		soul_op_divi,
		soul_op_printi,
		soul_op_halt,
	};
	// ((((54 + 45) + 1) * 12) / (8 / 4)) = 100 * 12 / 4 = 300

	soul_chunk_t chunk = soul_chunk_create();
	chunk.code = code;
	chunk.code_size = sizeof(code) / sizeof(code[0]);
	chunk.constants = constants;

	soul_vm_t vm = soul_vm_create();
	soul_vm_interpret(&vm, &chunk);
}

#if 0

typedef struct file_t file_t;
struct file_t
{
	char* data;
	size_t size;
	bool is_valid;
};

file_t read_file(const char* path)
{
	file_t file = {
		.data = NULL,
		.size = 0,
		.is_valid = false
	};

	FILE* f = fopen(path, "rb");
	if(!f) {
		printf("Failed to read a file: '%s'\n", path);
		return file;
	}

	// Get the file size.
	fseek(f, 0L, SEEK_END);
	file.size = ftell(f);
	rewind(f);
	
	// Alloc buffer
	file.data = (char*)malloc(file.size);
	if(!file.data) {
		printf("Failed to allocate a sufficient buffer to a file.\n");
		fclose(f);
		return file;
	}
	memset(file.data, '\0', sizeof(char) * file.size);

	// Read the whole file
	size_t read_size = fread(file.data, sizeof(char), file.size, f);
	if(read_size != file.size)
	{
		printf("Read size (%zu) is diffrenet from file's size (%zu).\n", read_size, file.size);
		fclose(f);
		return file;
	}

	// Close the file and read.
	fclose(f);
	file.is_valid = true;
	return file;
}

int main (void)
{
	file_t file = read_file("../../playground/test.soul");
	if(!file.is_valid) { return 1; }

	//
	// Lexing
	//

	soul_lexer_t lexer = soul_lexer_create();
	soul_token_array_t array = soul_lexer_scan(&lexer, file.data, file.size);

#ifdef SOUL_PRINT_ARRAY
	for(size_t i = 0; i < array.size; ++i)
	{
		const soul_token_t token = array.tokens[i];
		printf("%d: %.*s [%s]\n", i, (int)token.length, token.start, soul_token_type_to_string(token.type));
	}
#endif // SOUL_PRINT_ARRAY

	//
	// Parsing
	//

	soul_parser_t parser = soul_parser_create();
	soul_ast_node_t* ast = soul_parser_parse(&parser, &array);
	if(!ast)
	{
		printf("Failed to parse an array.\n");
		return 1;
	}

#ifdef SOUL_PRINT_AST
	soul_ast_node_stringify(ast);
#endif // SOUL_PRINT_AST

	//
	// Compiling
	//

	soul_compiler_t compiler = soul_compiler_create();
	soul_chunk_t chunk = soul_compiler_compile(&compiler, ast);

#ifdef SOUL_PRINT_BYTECODE
	for(size_t i = 0; chunk.code_size; ++i)
	{
	}
#endif // SOUL_PRINT_BYTECODE

	return 0;
}
#endif
