#ifndef SOUL_AST_TYPE_CHECKER_H
#define SOUL_AST_TYPE_CHECKER_H

#include "allocator.h"
#include "ast/ast.h"

#include <stdint.h>
#include <stdbool.h>

// TODO: Naming?
typedef enum soul_type_checker_flag_t : uint8_t
{
	soul_type_checker_flag_none         = 1 << 0,
	soul_type_checker_flag_strict_casts = 1 << 1,
} soul_type_checker_flag_t;

/** Creates type-checker ready for type-checking. */
typedef struct soul_type_checker_t soul_type_checker_t;
struct soul_type_checker_t
{
	soul_type_checker_flag_t flags;

	bool had_error;
	bool had_panic;

	soul_allocator_t* allocator;
};

/** */
soul_type_checker_t soul_type_checker_create(soul_allocator_t* allocator);

/** */
// TODO: Describe type checking algorithm as a doxygen.
bool soul_type_checker_check(soul_type_checker_t* type_checker, soul_ast_node_t* node);

#endif // SOUL_AST_TYPE_CHECKER_H
