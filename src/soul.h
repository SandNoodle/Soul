#ifndef SOUL_H
#define SOUL_H

// @TEMP @DEBUG
#define DEBUG() printf("[DEBUG] %s:%d: %s\n", __FILE__, __LINE__, __func__); fflush(stdout);

#define SOUL_VERSION_MAJOR 0
#define SOUL_VERSION_MINOR 0
#define SOUL_VERSION_PATCH 0

#define SOUL_VERSION_STRING "0.0.0"

#define SOUL_VERSION_NUMBER         \
	(SOUL_VERSION_MAJOR * 1000000 + \
	 SOUL_VERSION_MINOR * 1000 +    \
	 SOUL_VERSION_PATCH)

// ----------------------------------------------------------------------------
// Includes
// ----------------------------------------------------------------------------

#include "soul_config.h"

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

typedef bool soul_valid_t;

#define SOUL_ARRAY_SIZE(array) sizeof(array) / sizeof(array[0])

// @TODO @TEMP
#include <assert.h>
#define SOUL_UNIMPLEMENTED() assert(false && "UNIMPLEMENTED")
#define SOUL_UNUSED(x) (void)(x)
//

// ----------------------------------------------------------------------------
// Platform
// ----------------------------------------------------------------------------

// Windows
#if defined(WIN32) || defined(_WIN32) || defined(__WIN32__)
	#define SOUL_PLATFORM_WINDOWS 1
	#ifndef _WIN64
		#error "Windows 32-bit is not supported!"
	#endif
#endif

// Linux
#if defined(__linux__) || defined(__gnu_linux__)
	#define SOUL_PLATFORM_LINUX 1
#endif

// Unix
#if defined(__UNIX__)
	#define SOUL_PLATFORM_UNIX 1
#endif

// Mac
#if defined(__APPLE__)
	#define SOUL_PLATFORM_MAC 1
#endif

// ----------------------------------------------------------------------------
// @TODO STATIC / DYNAMIC (dllexport, dllimport)
// ----------------------------------------------------------------------------

#define SOUL_API

// ----------------------------------------------------------------------------
// Forward Declarations
// ----------------------------------------------------------------------------

typedef void* (*soul_allocate_fn)(void* memory, size_t new_size, void* user_data);

typedef void (*soul_message_callback_t)(const char* file, uint32_t line, const char* message, size_t length);

typedef struct soul_vm_t soul_vm_t;

typedef struct soul_chunk_t soul_chunk_t;

typedef struct soul_token_t soul_token_t;

typedef struct soul_ast_t soul_ast_t;

// ----------------------------------------------------------------------------
// Declarations
// ----------------------------------------------------------------------------

// @TODO STRING AND CHAR
typedef enum {
	SOUL_VAL_B8,
	SOUL_VAL_U8,  SOUL_VAL_I8,
	SOUL_VAL_U16, SOUL_VAL_I16,
	SOUL_VAL_U32, SOUL_VAL_I32,
	SOUL_VAL_U64, SOUL_VAL_I64,
	SOUL_VAL_F32, SOUL_VAL_F64,
} soul_value_type_t;

// @TODO STRING AND CHAR
typedef struct{
	soul_value_type_t type;
	union {
		uint8_t  b8;
		uint8_t  u8;
		uint16_t u16;
		uint32_t u32;
		uint64_t u64;
		int8_t   i8;
		int16_t  i16;
		int32_t  i32;
		int64_t  i64;
		float    f32;
		double   f64;
	// @TODO STRING AND CHAR
	} as;
} soul_value_t;

typedef soul_value_t soul_register_t;

// @TODO Poor man's vector inspired by Wren implementation.
//       Replace realloc with user provided allocator.
#define SOUL_GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity) * 2)
#define SOUL_GROW_ARRAY(type, ptr, capacity) (type*)realloc((ptr), (capacity) * sizeof(type))

#define SOUL_VECTOR_DEFINE(name, type)                                   \
	typedef struct {                                                     \
		type* data;                                                      \
		size_t size;                                                     \
		size_t capacity;                                                 \
		soul_valid_t valid;                                              \
	} soul_##name##_vector_t;                                            \
                                                                         \
	void soul__new_##name##_vector(soul_##name##_vector_t* vector);      \
	void soul__free_##name##_vector(soul_##name##_vector_t* vector);     \
	void soul__##name##_vector_push(soul_##name##_vector_t* v, type t);

#define SOUL_VECTOR_DECLARE(name, type)                                  \
	void soul__new_##name##_vector(soul_##name##_vector_t* v)            \
	{                                                                    \
		v->size = 0;                                                     \
		v->capacity = SOUL_GROW_CAPACITY(0);                             \
		v->data = SOUL_GROW_ARRAY(type, NULL, v->capacity);              \
		v->valid = true;                                                 \
	}                                                                    \
                                                                         \
	void soul__free_##name##_vector(soul_##name##_vector_t* v)           \
	{                                                                    \
		free(v->data);                                                   \
		v->data = NULL;                                                  \
		v->size = 0;                                                     \
		v->capacity = 0;                                                 \
		v->valid = false;                                                \
	}                                                                    \
                                                                         \
	void soul__##name##_vector_push(soul_##name##_vector_t* v, type t)   \
	{                                                                    \
		if(v->size + 1 > v->capacity)                                    \
		{                                                                \
			v->capacity = SOUL_GROW_CAPACITY(v->capacity);               \
			v->data = SOUL_GROW_ARRAY(type, v->data, v->capacity);       \
		}                                                                \
		v->data[v->size++] = t;                                          \
	}

// ----------------------------------------------------------------------------
// PUBLIC API
// ----------------------------------------------------------------------------

// @TODO Dont pass by value to functions.
//       Might be slow and memory consuming.
//       Benchmark first!

typedef enum {
	SOUL_SUCCESS,       // Runtime success.
	SOUL_PARSE_ERROR,   // Error while parsing the script.
	SOUL_COMPILE_ERROR, // Error while compiling the script.
	SOUL_RUNTIME_ERROR, // Error while running the script.
} soul_result_t;

typedef void* (*soul_allocate_fn)(void* memory, size_t new_size, void* user_data);

typedef void (*soul_message_callback_t)(const char* file, uint32_t line, const char* message, size_t length);

//
// Runtime
//

typedef struct soul_vm_t soul_vm_t;
typedef struct soul_chunk_t soul_chunk_t;

// This struct contains configuration options for a runtime enviroment.
typedef struct {
	size_t vm_stack_size_initial;
	size_t vm_stack_size_max;               // If 0, then maximum size will be unboud.

	soul_message_callback_t warn_callback;  // Can be null.
	soul_message_callback_t error_callback; // Can be null.
} soul_config_t;

SOUL_API soul_config_t soul_get_defualt_config(void);

SOUL_API void soul_vm_init(soul_vm_t* vm);
SOUL_API void soul_vm_free(soul_vm_t* vm);

SOUL_API soul_result_t soul_vm_interpret(soul_vm_t* vm, soul_chunk_t* chunk);

//
// Scanning
//
typedef struct {
	soul_message_callback_t warn_callback;  // Can be null.
	soul_message_callback_t error_callback; // Can be null.
} soul_scanner_config_t;

typedef struct soul_token_t soul_token_t;

SOUL_VECTOR_DEFINE(token, soul_token_t);

SOUL_API soul_scanner_config_t soul_get_defualt_scanner_config(void);

SOUL_API soul_token_vector_t soul_scan(const char* buffer, size_t size);

//
// Parsing
//

typedef struct {
	soul_message_callback_t warn_callback;  // Can be null.
	soul_message_callback_t error_callback; // Can be null.
} soul_parser_config_t;

typedef struct soul_ast_t soul_ast_t;

SOUL_API soul_parser_config_t soul_get_defualt_parser_config(void);

SOUL_API soul_ast_t* soul_parse(soul_token_vector_t tokens);

SOUL_API void soul_free_ast(soul_ast_t* ast);

//
// Compiling
//
typedef struct {

} soul_compiler_config_t;

SOUL_API soul_compiler_config_t soul_get_defualt_compiler_config(void);

SOUL_API soul_chunk_t soul_compile(soul_ast_t* ast);

//
// Serialization
//

SOUL_API soul_chunk_t soul_deserialize(const char* path);
SOUL_API soul_valid_t soul_serialize(soul_chunk_t chunk, const char* path);

#endif // SOUL_H
