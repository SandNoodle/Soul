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

#include "soul_fwd.h"
#include "soul_config.h"
#include "soul_containers.h"

#include <stdlib.h>
#include <string.h>

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
// Defines
// ----------------------------------------------------------------------------

typedef void* (*soul_allocate_fn)(void* memory, size_t new_size, void* user_data);

typedef void (*soul_message_callback_t)(const char* file, uint32_t line, const char* message, size_t length);

SOUL_VECTOR_DEFINE(chunk_constants, soul_value_t)
SOUL_VECTOR_DEFINE(chunk_data, uint8_t)
SOUL_VECTOR_DEFINE(token, soul_token_t)

// ----------------------------------------------------------------------------
// PUBLIC API
// ----------------------------------------------------------------------------

typedef enum {
	SOUL_SUCCESS,       // Generic success.
	SOUL_PARSE_ERROR,   // Error while parsing the script.
	SOUL_COMPILE_ERROR, // Error while compiling the script.
	SOUL_RUNTIME_ERROR, // Error while running the script.
} soul_result_t;

//
// Runtime
//

SOUL_API soul_config_t soul_get_defualt_config(void);

SOUL_API void soul_vm_init(soul_vm_t* vm);
SOUL_API void soul_vm_free(soul_vm_t* vm);

SOUL_API soul_result_t soul_vm_interpret(soul_vm_t* vm, soul_chunk_t* chunk);

//
// Scanning
//

SOUL_API soul_scanner_config_t soul_get_defualt_scanner_config(void);

SOUL_API soul_token_vector_t soul_scan(const char* buffer, size_t size);

//
// Parsing
//

SOUL_API soul_parser_config_t soul_get_defualt_parser_config(void);

SOUL_API soul_ast_t* soul_parse(soul_token_vector_t tokens);

SOUL_API void soul_free_ast(soul_ast_t* ast);

//
// Compiling
//

SOUL_API soul_compiler_config_t soul_get_defualt_compiler_config(void);

SOUL_API soul_chunk_t* soul_compile(soul_ast_t* ast);

//
// Serialization
//

SOUL_API soul_chunk_t* soul_deserialize(const char* path);

SOUL_API bool soul_serialize(soul_chunk_t* chunk, const char* path);

#endif // SOUL_H
