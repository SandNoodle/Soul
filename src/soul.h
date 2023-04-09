#ifndef SOUL_H
#define SOUL_H

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

#include <stdlib.h>
#include <stdint.h>
#include <stdbool.h>
#include <string.h>

typedef bool soul_valid_t;

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
// Declarations
// ----------------------------------------------------------------------------

typedef enum {
	VAL_B8,
	VAL_U8,  VAL_I8,
	VAL_U16, VAL_I16,
	VAL_U32, VAL_I32,
	VAL_U64, VAL_I64,
	VAL_F32, VAL_F64,
	// @TODO STRING AND CHAR
} soul_value_type_t;

typedef struct{
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

// ----------------------------------------------------------------------------
// PUBLIC API
// ----------------------------------------------------------------------------

typedef enum {
	SOUL_SUCCESS,       // Runtime success.
	SOUL_PARSE_ERROR,   // Error while parsing the script.
	SOUL_COMPILE_ERROR, // Error while compiling the script.
	SOUL_RUNTIME_ERROR, // Error while running the script.
} soul_result_t;

typedef void* (*soul_allocate_fn)(void* memory, size_t new_size);

typedef void (*soul_message_callback_t)(const char* file, uint32_t line, const char* message, size_t length);

//
// Runtime
//

typedef struct soul_vm_t soul_vm_t;

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

//
// Scanning
//
typedef struct {
	soul_message_callback_t warn_callback;  // Can be null.
	soul_message_callback_t error_callback; // Can be null.
} soul_scanner_config_t;

typedef struct soul_token_array_t soul_token_array_t;

SOUL_API soul_scanner_config_t soul_get_defualt_scanner_config(void);

SOUL_API soul_token_array_t soul_scan(const char* buffer, size_t size);

//
// Parsing
//

typedef struct {
	soul_message_callback_t warn_callback;  // Can be null.
	soul_message_callback_t error_callback; // Can be null.
} soul_parser_config_t;

typedef struct soul_ast_t soul_ast_t;

SOUL_API soul_parser_config_t soul_get_defualt_parser_config(void);

SOUL_API soul_ast_t soul_parse(soul_token_array_t tokens);

SOUL_API void soul_free_ast(soul_ast_t ast);

//
// Compiling
//
typedef struct {

} soul_compiler_config_t;

SOUL_API soul_compiler_config_t soul_get_defualt_compiler_config(void);

typedef struct soul_chunk_t soul_chunk_t;

SOUL_API soul_chunk_t soul_compile(soul_ast_t ast);

//
// Serialization
//

SOUL_API soul_chunk_t soul_deserialize(const char* path);
SOUL_API soul_valid_t soul_serialize(soul_chunk_t chunk, const char* path);

#endif // SOUL_H
