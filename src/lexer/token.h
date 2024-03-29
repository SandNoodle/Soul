#ifndef LEXER_TOKEN_H
#define LEXER_TOKEN_H

#include "allocator.h"

#include <stdbool.h>
#include <stdint.h>

typedef enum soul_token_type_t : uint8_t
{
	// clang-format off
	soul_token_unknown, // Presence of an unknown token should convert into an
	                    // error if we cannot resolve it (somehow).

	// Single character tokens
	soul_token_semicolon, soul_token_question_mark,    // ;?
	soul_token_percent, soul_token_caret,              // %^
	soul_token_dot, soul_token_comma,                  // .,
	soul_token_paren_left, soul_token_paren_right,     // ()
	soul_token_brace_left, soul_token_brace_right,     // {}
	soul_token_bracket_left, soul_token_bracket_right, // []

	// One or two character tokens
	soul_token_colon, soul_token_double_colon,                         // : ::
	soul_token_equal, soul_token_double_equal,                         // = ==
	soul_token_bang, soul_token_bang_equal,                            // ! !=
	soul_token_greater, soul_token_greater_equal,                      // > >=
	soul_token_less, soul_token_less_equal,                            // < <=
	soul_token_plus, soul_token_plus_equal, soul_token_double_plus,    // + += ++
	soul_token_minus, soul_token_minus_equal, soul_token_double_minus, // - -= --
	soul_token_star, soul_token_star_equal,                            // * *=
	soul_token_slash, soul_token_slash_equal,                          // / /=
	soul_token_ampersand, soul_token_double_ampersand,                 // & &&
	soul_token_pipe, soul_token_double_pipe,                           // | ||

	// Literals
	soul_token_number,     // ex. 123, 3.14
	soul_token_string,     // ex. "test_string", 'c'
	soul_token_identifier, // ex. my_identifier

	// Keywords
	soul_token_native,                     // Native C function
	soul_token_import,                     // File import
	soul_token_define,                     // Define alias
	soul_token_let, soul_token_mut,        // Variables
	soul_token_if, soul_token_else,        // Flow control
	soul_token_for, soul_token_while,      // Loops
	soul_token_continue, soul_token_break, // Loop keywords
	soul_token_return, soul_token_fn,      // Function
	soul_token_struct, soul_token_enum,    // Data types
	soul_token_true, soul_token_false,     // Truthness literals

	// Special tokens
	soul_token_error, // Token containing error message.
	soul_token_eof,   // Token signaling End of File.
	// clang-format off
} soul_token_type_t;

typedef struct soul_token_t soul_token_t;
struct soul_token_t
{
	soul_token_type_t type;
	const char* start;
	size_t length;
};

typedef struct soul_token_keyword_t soul_token_keyword_t;
struct soul_token_keyword_t
{
	soul_token_type_t type;
	const char* start;
	size_t      length;
};

#define soul_token_keywords_size 16
static const soul_token_keyword_t soul_keywords[soul_token_keywords_size] = {
	{ soul_token_native,   "native",   6 },
	{ soul_token_import,   "import",   6 },
	{ soul_token_define,   "define",   6 },
	{ soul_token_fn,       "fn",       2 },
	{ soul_token_let,      "let",      3 },
	{ soul_token_mut,      "mut",      3 },
	{ soul_token_if,       "if",       2 },
	{ soul_token_else,     "else",     4 },
	{ soul_token_for,      "for",      3 },
	{ soul_token_while,    "while",    5 },
	{ soul_token_continue, "continue", 8 },
	{ soul_token_break,    "break",    5 },
	{ soul_token_struct,   "struct",   6 },
	{ soul_token_enum,     "enum",     4 },
	{ soul_token_true,     "true",     4 },
	{ soul_token_false,    "false",    5 },
};

/** Converts given token type as a string. */
const char* soul_token_type_to_string(soul_token_type_t type);

/** Returns true if given token's type is of a literal type. */
bool soul_is_literal_token(soul_token_type_t type);

/** Returns true if given token's type is of an assign type. */
bool soul_is_assign_token(soul_token_type_t type);

/** Returns true if given token's type is a synchronization type. */
bool soul_is_sync_token(soul_token_type_t type);

typedef struct soul_token_array_t soul_token_array_t;
struct soul_token_array_t
{
	soul_token_t* tokens;
	size_t size;
	size_t capacity;

	soul_allocator_t* allocator;
};

/** Creates a dynamic array struct that can holds tokens. */
soul_token_array_t soul_token_array_create(soul_allocator_t* allocator);

/** Destroys given soul_token_array */
void soul_token_array_destroy(soul_token_array_t* array);

/** Appends the given token at the end of an array. */
bool soul_token_array_append(soul_token_array_t* array, soul_token_t token);

/** Returns token at an index. */
soul_token_t soul_token_array_at(soul_token_array_t* tokens, size_t index);

/** Returns type of a token at an index. */
soul_token_type_t soul_token_array_type_at(soul_token_array_t* tokens,
                                           size_t index);

/** Returns type of a token currently stored at the end of an array */
soul_token_type_t soul_token_array_type_back(soul_token_array_t* array);

#endif // LEXER_TOKEN_H
