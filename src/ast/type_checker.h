#ifndef SOUL_AST_TYPE_CHECKER_H
#define SOUL_AST_TYPE_CHECKER_H

#include <stdint.h>

struct soul_ast_node_t;

/** Creates type-checker ready for type-checking. */
typedef struct soul_type_checker_t soul_type_checker_t;
struct soul_type_checker_t
{
};

typedef enum soul_type_checker_status_t
{
	soul_type_checker_status_ok,
	soul_type_checker_status_error,
} soul_type_checker_status_t;

typedef struct soul_type_checker_result_t soul_type_checker_result_t;
struct soul_type_checker_result_t
{
	soul_type_checker_status_t status;

	const char* error_message;
	size_t error_message_length;
};

/** Creates an AST type checker ready for use. */
soul_type_checker_t soul_type_checker_create(void);

/**
 * @brief Performs type checking on a given AST.
 *
 * @param type_checker @TODO
 * @param root root of the AST.
 * @return result of the type checking.
 */
soul_type_checker_result_t soul_type_checker_analyze(soul_type_checker_t* type_checker, struct soul_ast_node_t* root);

#endif // SOUL_AST_TYPE_CHECKER_H
