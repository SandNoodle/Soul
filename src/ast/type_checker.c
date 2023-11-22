#include "type_checker.h"

#include "ast/ast.h"

#include <stdlib.h>

struct soul_type_checker_t
{
	soul_ast_node_t* root;
};

soul_type_checker_t* soul_type_checker_create(void)
{
	soul_type_checker_t* t = (soul_type_checker_t*)malloc(sizeof(soul_type_checker_t));
	t->root = NULL;
	return t;
}

void soul_type_checker_destroy(soul_type_checker_t* t)
{
	if(!t) return;
	free(t);
}

soul_type_checker_result_t soul_type_checker_analyze(struct soul_ast_node_t* root)
{
	return soul_type_checker_ok;
}
