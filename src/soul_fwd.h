#ifndef SOUL_FWD_H
#define SOUL_FWD_H

#include <stdbool.h>

//
// API
//

typedef struct soul_config_t soul_config_t;

typedef struct soul_scanner_config_t soul_scanner_config_t;

typedef struct soul_parser_config_t soul_parser_config_t;

typedef struct soul_compiler_config_t soul_compiler_config_t;

typedef struct soul_value_t soul_value_t;

typedef struct soul_vm_t soul_vm_t;

typedef struct soul_chunk_t soul_chunk_t;

typedef struct soul_token_t soul_token_t;

typedef struct soul_ast_t soul_ast_t;

//
// Objects
//

typedef struct soul_obj_t soul_obj_t;

typedef struct soul_function_obj_t soul_function_obj_t;

typedef struct soul_string_obj_t soul_string_obj_t;

//
// Containers
//

typedef struct soul_hashtable_entry_t soul_hashtable_entry_t;

typedef struct soul_hashtable_t soul_hashtable_t;

#endif //  SOUL_FWD_H
