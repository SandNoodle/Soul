#ifndef SOUL_AST_TYPE_CHECKER_H
#define SOUL_AST_TYPE_CHECKER_H

struct soul_ast_node_t;

/** */
typedef struct soul_type_checker_t soul_type_checker_t;

typedef enum soul_type_checker_result_t {
	soul_type_checker_ok,
} soul_type_checker_result_t;

/** Creates an AST type checker ready for use. */
soul_type_checker_t* soul_type_checker_create(void);

/** Destroys given type checker. */
void soul_type_checker_destroy(soul_type_checker_t*);

/**
 * @brief Performs type checking on a given AST.
 *
 * @param root root of the AST.
 * @return result of the type checking.
 */
soul_type_checker_result_t soul_type_checker_analyze(struct soul_ast_node_t* root);

#endif // SOUL_AST_TYPE_CHECKER_H
