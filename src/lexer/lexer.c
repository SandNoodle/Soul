#include "lexer.h"

#include "lexer/token.h"

#include <stdlib.h>
#include <string.h>

static soul_token_t lexer_scan_token(soul_lexer_t*);
static soul_token_t lexer_create_token(soul_lexer_t*, soul_token_type_t);
static soul_token_t lexer_create_error_token(soul_lexer_t*, const char*);
static soul_token_t lexer_create_identifier_token(soul_lexer_t*);
static soul_token_t lexer_create_numeric_token(soul_lexer_t*);
static soul_token_t lexer_create_string_token(soul_lexer_t*);
static void lexer_skip_whitespace(soul_lexer_t*);
static char lexer_advance(soul_lexer_t*);
static char lexer_peek(soul_lexer_t*);
static bool lexer_match_next(soul_lexer_t*, char c);
static bool is_eof(char c);
static bool is_alpha(char c);
static bool is_digit(char c);
static bool is_quotation(char c);

soul_lexer_t soul_lexer_create(void)
{
	soul_lexer_t lexer;
	lexer.token_start  = NULL;
	lexer.current_char = NULL;
	lexer.current_line = 0;
	return lexer;
}

soul_token_array_t soul_lexer_scan(soul_lexer_t* lexer, const char* str,
                                   size_t size)
{
	if (!lexer) return soul_token_array_create();

	lexer->token_start  = str;
	lexer->current_char = str;

	soul_token_array_t array = soul_token_array_create();
	for (;;)
	{
		soul_token_t token = lexer_scan_token(lexer);
		soul_token_array_append(&array, token);
		if (soul_token_array_type_back(&array) == soul_token_eof) break;
	}

	return array;
}

//
// Private
//

static soul_token_t lexer_scan_token(soul_lexer_t* lexer)
{
	lexer_skip_whitespace(lexer);

	char c = lexer_advance(lexer);
	if (is_eof(c)) return lexer_create_token(lexer, soul_token_eof);
	if (is_alpha(c)) return lexer_create_identifier_token(lexer);
	if (is_digit(c)) return lexer_create_numeric_token(lexer);
	if (is_quotation(c)) return lexer_create_string_token(lexer);

	switch (c)
	{
		// clang-format off
		// Single character tokens
		#define MAKE_SINGLE_CHAR_TOKEN(type) return lexer_create_token(lexer, (type))
		case ';': MAKE_SINGLE_CHAR_TOKEN(soul_token_semicolon);
		case '?': MAKE_SINGLE_CHAR_TOKEN(soul_token_question_mark);
		case '%': MAKE_SINGLE_CHAR_TOKEN(soul_token_percent);
		case '.': MAKE_SINGLE_CHAR_TOKEN(soul_token_dot);
		case ',': MAKE_SINGLE_CHAR_TOKEN(soul_token_comma);
		case '(': MAKE_SINGLE_CHAR_TOKEN(soul_token_paren_left);
		case ')': MAKE_SINGLE_CHAR_TOKEN(soul_token_paren_right);
		case '{': MAKE_SINGLE_CHAR_TOKEN(soul_token_brace_left);
		case '}': MAKE_SINGLE_CHAR_TOKEN(soul_token_brace_right);
		case '[': MAKE_SINGLE_CHAR_TOKEN(soul_token_bracket_left);
		case ']': MAKE_SINGLE_CHAR_TOKEN(soul_token_bracket_right);

		// One or two character tokens
		#define MAKE_MULTI_CHAR_TOKEN(next_char, type_match, type_not_match)           \
			return lexer_create_token(lexer, lexer_match_next(lexer, (next_char))      \
	                                     ? (type_match)                                \
	                                     : (type_not_match))
		case ':': MAKE_MULTI_CHAR_TOKEN(':', soul_token_double_colon, soul_token_colon);
		case '=': MAKE_MULTI_CHAR_TOKEN('=', soul_token_double_equal, soul_token_equal);
		case '!': MAKE_MULTI_CHAR_TOKEN('=', soul_token_bang_equal, soul_token_bang);
		case '>': MAKE_MULTI_CHAR_TOKEN('=', soul_token_greater_equal, soul_token_greater);
		case '<': MAKE_MULTI_CHAR_TOKEN('=', soul_token_less_equal, soul_token_less);
		case '+': MAKE_MULTI_CHAR_TOKEN('=', soul_token_plus_equal, soul_token_plus);
		case '-': MAKE_MULTI_CHAR_TOKEN('=', soul_token_minus_equal, soul_token_minus);
		case '*': MAKE_MULTI_CHAR_TOKEN('=', soul_token_star_equal, soul_token_star);
		case '/': MAKE_MULTI_CHAR_TOKEN('=', soul_token_slash_equal, soul_token_slash);
		case '&': MAKE_MULTI_CHAR_TOKEN('&', soul_token_double_ampersand, soul_token_ampersand);
		case '|': MAKE_MULTI_CHAR_TOKEN('|', soul_token_double_pipe, soul_token_pipe);
		// clang-format on
		default:
			break;
	}

	return lexer_create_error_token(lexer, "unrecognized token");
}

static soul_token_t lexer_create_token(soul_lexer_t* lexer,
                                       soul_token_type_t type)
{
	soul_token_t token
	    = {.type   = type,
	       .start  = lexer->token_start,
	       .length = (size_t)(lexer->current_char - lexer->token_start)};
	return token;
}

static soul_token_t lexer_create_error_token(soul_lexer_t* lexer,
                                             const char* message)
{
	soul_token_t token = {
	    .type   = soul_token_error,
	    .start  = message,
	    .length = strlen(message),
	};
	return token;
}

static soul_token_t lexer_create_identifier_token(soul_lexer_t* lexer)
{
	bool peek_next_char = true;
	while (peek_next_char)
	{
		lexer_advance(lexer);
		char c         = lexer_peek(lexer);
		peek_next_char = is_alpha(c) || is_digit(c);
	}

	// Check for keywords.
	const size_t length = lexer->current_char - lexer->token_start;
	for (size_t index = 0; index < soul_token_keywords_size; ++index)
	{
		soul_token_keyword_t keyword = soul_keywords[index];
		bool same_size               = length == keyword.length;
		bool same_string
		    = memcmp(lexer->token_start, keyword.start, length) == 0;
		if (same_size && same_string)
		{
			return lexer_create_token(lexer, keyword.type);
		}
	}

	return lexer_create_token(lexer, soul_token_identifier);
}

// @TODO This function currently supports only parsing integers.
//       Add support for parsing: floating point numbers (IEE754), binary, hex.
static soul_token_t lexer_create_numeric_token(soul_lexer_t* lexer)
{
	while (is_digit(lexer_peek(lexer))) { lexer_advance(lexer); }

	return lexer_create_token(lexer, soul_token_number);
}

static soul_token_t lexer_create_string_token(soul_lexer_t* lexer)
{
	char c = lexer_peek(lexer);
	while (!is_quotation(c) && !is_eof(c))
	{
		if (c == '\n') lexer->current_line++;
		lexer_advance(lexer);
		c = lexer_peek(lexer);
	}

	if (is_eof(lexer_peek(lexer)))
	{
		return lexer_create_error_token(lexer, "unterminated string");
	}

	lexer_advance(lexer);
	return lexer_create_token(lexer, soul_token_string);
}

static void lexer_skip_whitespace(soul_lexer_t* lexer)
{
	for (;;)
	{
		char c = lexer_peek(lexer);
		switch (c)
		{
			case ' ':
			case '\r':
			case '\t':
				lexer_advance(lexer);
				break;
			case '\n':
				lexer_advance(lexer);
				lexer->current_line++;
				break;
			case '#':                 // Comments
				lexer_advance(lexer); // remove '#'
				c = lexer_peek(lexer);
				while (c != '\n' && c != '\r' && !is_eof(c))
				{
					lexer_advance(lexer);
					c = lexer_peek(lexer);
				}
				break;
			default:
				return;
		}
	}
}

static char lexer_advance(soul_lexer_t* lexer)
{
	return *(lexer->current_char++);
}

static char lexer_peek(soul_lexer_t* lexer) { return *lexer->current_char; }

static bool lexer_match_next(soul_lexer_t* lexer, char expected)
{
	char next = lexer_peek(lexer);
	if (is_eof(next) || next != expected) return false;

	lexer->current_char++;
	return true;
}

static bool is_digit(char c) { return (c >= '0' && c <= '9'); }

static bool is_alpha(char c)
{
	return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

static bool is_eof(char c) { return c == '\0'; }

static bool is_quotation(char c) { return c == '"'; }
