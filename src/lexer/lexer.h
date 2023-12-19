#ifndef SOUL_LEXER_LEXER_H
#define SOUL_LEXER_LEXER_H

#include <stdint.h>

struct soul_token_array_t;
typedef struct soul_lexer_t soul_lexer_t;
struct soul_lexer_t
{
	const char* token_start;
	const char* current_char;
	uint32_t current_line;
};

/** Creates a lexer ready for scanning. */
soul_lexer_t soul_lexer_create(void);

/**
 * @brief Scans given string and converts it into a array of tokens.
 *
 * @param lexer lexer to use for scanning.
 * @param str string to scan.
 * @param size size of the string.
 * @return array of scanned tokens if successful, or nullptr otherwise.
 */
struct soul_token_array_t soul_lexer_scan(soul_lexer_t* lexer, const char* str, size_t size);

#endif // SOUL_LEXER_LEXER_H
