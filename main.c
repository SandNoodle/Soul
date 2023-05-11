#include <stdio.h>

#include "src/soul.h"

#include "src/token.c"
#include "src/scanner.c"
#include "src/parser.c"
#include "src/ast.c"
#include "src/opcode.c"
#include "src/compiler.c"
#include "src/vm.c"

// @TODO Currently working on parsing variable declarations.
//       Parse based on type. Currently hardcoded I64.
// @TODO Currently working on swaping all return types
//       to pointers instead of return by value.

int main()
{
	// READ FILE HELPER
	int read_size, buffer_size;
	FILE* file = fopen("./examples/test.soul", "rb");
	if(!file) { printf("Invalid file.\n"); return 1; }

	fseek(file, 0, SEEK_END);
	buffer_size = ftell(file);
	rewind(file);

	char* buffer = (char*) malloc(sizeof(char) * (buffer_size + 1) );
	read_size = fread(buffer, sizeof(char), buffer_size, file);

	buffer[buffer_size] = '\0';

	if (buffer_size != read_size)
	{
		free(buffer);
		buffer = NULL;
		printf("Fucky wacky\n");
		return 1;
	}
	fclose(file);
	//

	soul_token_vector_t array = soul_scan(buffer, buffer_size);
	soul_print_token_array(&array, true);
	soul_ast_t* ast = soul_parse(array);
	if(!ast->valid)
	{
		printf("[ERROR] Invalid AST!");
		return 1;
	}

	soul__ast_print(ast);

	soul_chunk_t chunk = soul_compile(ast);
	if(!chunk.valid)
	{
		printf("[ERROR] Invalid Chunk!");
		return 1;
	}

	soul_vm_t vm;
	soul_vm_init(&vm);

	soul_result_t runtime_result = soul_vm_interpret(&vm, &chunk);
	if(runtime_result != SOUL_SUCCESS)
	{
		printf("[ERROR] Runtime error!");
		return 1;
	}

	soul_vm_free(&vm);

	return 0;
}
