#pragma once

#include "soul.h"

typedef enum {
	#define TOKEN(x) x,
	#include "token.inl"
} soul_token_type_t;

typedef struct {
	soul_token_type_t type;
	const char* name;
	size_t length;
} soul_token_keyword_t;

#define KEYWORDS_LENGTH 18
static const soul_token_keyword_t keywords[KEYWORDS_LENGTH] =
{
	{ TOKEN_NATIVE,    "native",    6 },
	{ TOKEN_IMPORT,    "import",    6 },
	{ TOKEN_IMPORT,    "export",    6 },
	{ TOKEN_DEFINE,    "define",    6 },
	{ TOKEN_LET,       "let",       3 },
	{ TOKEN_MUT,       "mut",       3 },
	{ TOKEN_IF,        "if",        2 },
	{ TOKEN_ELSE,      "else",      4 },
	{ TOKEN_FOR,       "for",       3 },
	{ TOKEN_WHILE,     "while",     5 },
	{ TOKEN_RETURN,    "return",    6 },
	{ TOKEN_FN,        "fn",        2 },
	{ TOKEN_STRUCT,    "struct",    6 },
	{ TOKEN_ENUM,      "enum",      4 },
	{ TOKEN_TRUE,      "true",      4 },
	{ TOKEN_FALSE,     "false",     5 },
	{ TOKEN_CONTINUE , "continue",  8 },
	{ TOKEN_BREAK,     "break",     5 },
};

struct soul_token_t {
	soul_token_type_t type;
	const char* start;
	size_t length;

	uint32_t line;
};

// @TODO Move this out to soul.h
const char* soul_token_to_string(soul_token_type_t type);
void soul_print_token_array(soul_token_vector_t* array, bool pretty_print); // @TEMP
