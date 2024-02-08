#include "lexer/lexer.h"
#include "lexer/token.h"
#include "parser/parser.h"
#include "ast/ast.h"
#include "ast/ast_stringify.h"
#include "compiler/compiler.h"
#include "runtime/chunk.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SOUL_PRINT_ARRAY
#define SOUL_PRINT_AST
#define SOUL_PRINT_BYTECODE

// TODO: Working on printing the ast.
//       There seems a bug in parsing of a block statements - it does not end parsing.

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
