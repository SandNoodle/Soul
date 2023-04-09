#include "scanner.h"

#include <stdio.h>

#define SCANNER_PEEK() *scanner->current

static const soul_token_array_t soul__invalid_token_array = {
	.data  = NULL,
	.size = 0,
	.capacity = 0,
	.valid = false,
};

static bool soul__scanner_is_digit(char c)
{
	return (c >= '0' && c <= '9');
}

static bool soul__scanner_is_alpha(char c)
{
	return (c >= 'a' && c <= 'z')
		|| (c >= 'A' && c <= 'Z')
		||  c == '_';
}

static bool soul__scanner_is_eof(char c)
{
	return c == '\0';
}

// Consumes current character and returns it.
static char soul__scanner_advance(soul_scanner_t* scanner)
{
	scanner->current++;
	return scanner->current[-1];
}

static void soul__scanner_skip_whitespace(soul_scanner_t* scanner)
{
	for(;;)
	{
		char c = SCANNER_PEEK();
		switch(c)
		{
			case ' ':
			case '\r':
			case '\t':
				soul__scanner_advance(scanner);
				break;
			case '\n':
				soul__scanner_advance(scanner);
				scanner->line++;
				break;
			case '#': // Comments
				soul__scanner_advance(scanner); // remove '#'
				while( SCANNER_PEEK() != '\n'
					&& SCANNER_PEEK() != '\r'
					&& !soul__scanner_is_eof(SCANNER_PEEK()))
				{
					soul__scanner_advance(scanner);
				}
				break;
			default:
				return;
		}
	}
}

static bool soul__scanner_match_next(soul_scanner_t* scanner, char expected)
{
	if(soul__scanner_is_eof(SCANNER_PEEK())) return false;
	if(SCANNER_PEEK() != expected) return false;

	scanner->current++;
	return true;
}

static soul_token_t soul__scanner_make_token(soul_scanner_t* scanner, soul_token_type_t type)
{
	const soul_token_t token = {
		.type = type,
		.start = scanner->start,
		.length = (scanner->current - scanner->start),
		.line = scanner->line,
	};

	return token;
}

static soul_token_t soul__scanner_make_error_token(soul_scanner_t* scanner, const char* message)
{
	const soul_token_t token = {
		.type = TOKEN_ERROR,
		.start = message,
		.length = strlen(message),
		.line = scanner->line,
	};

	return token;
}

static soul_token_t soul__scanner_make_string_token(soul_scanner_t* scanner)
{
	while(SCANNER_PEEK() != '"' && !soul__scanner_is_eof(SCANNER_PEEK()))
	{
		if(SCANNER_PEEK() == '\n')
		{
			scanner->line++;
		}

		soul__scanner_advance(scanner);
	}

	if(soul__scanner_is_eof(SCANNER_PEEK()))
		return soul__scanner_make_error_token(scanner, "String was not terminated ('\"').");

	soul__scanner_advance(scanner);
	return soul__scanner_make_token(scanner, TOKEN_STRING);
}

static soul_token_t soul__scanner_make_identifier_token(soul_scanner_t* scanner)
{
	while(soul__scanner_is_alpha(SCANNER_PEEK()) || soul__scanner_is_digit(SCANNER_PEEK()))
	{
		soul__scanner_advance(scanner);
	}

	const size_t length = scanner->current - scanner->start;
	for(size_t index = 0; index < KEYWORDS_LENGTH; ++index)
	{
		const soul_token_keyword_t keyword = keywords[index];
		const bool same_size = length == keyword.length;
		const bool same_string = memcmp(scanner->start, keyword.name, length) == 0;
		if(same_size && same_string)
		{
			return soul__scanner_make_token(scanner, keyword.type);
		}
	}

	return soul__scanner_make_token(scanner, TOKEN_IDENTIFIER);
}

// @TODO NUMBER TOKENS
static soul_token_t soul__scanner_make_number_token(soul_scanner_t* scanner)
{
	while(soul__scanner_is_digit(SCANNER_PEEK())) soul__scanner_advance(scanner);

	// Consume the '.'
	if(SCANNER_PEEK() == '.' && soul__scanner_is_digit(SCANNER_PEEK()))
	{
		soul__scanner_advance(scanner);

		while(soul__scanner_is_digit(SCANNER_PEEK())) soul__scanner_advance(scanner);
	}

	return soul__scanner_make_token(scanner, TOKEN_NUMBER);
}

static soul_token_t soul__scanner_scan_token(soul_scanner_t* scanner)
{
	soul__scanner_skip_whitespace(scanner);
	scanner->start = scanner->current;

	char c = soul__scanner_advance(scanner);

	if(soul__scanner_is_eof(c)) return soul__scanner_make_token(scanner, TOKEN_EOF);

	if(soul__scanner_is_alpha(c)) return soul__scanner_make_identifier_token(scanner);
	if(soul__scanner_is_digit(c)) return soul__scanner_make_number_token(scanner);

	switch(c)
	{
		// String literals
		case '"': return soul__scanner_make_string_token(scanner);

		// Single character tokens
		#define MAKE_SINGLE_CHAR_TOKEN(token) return soul__scanner_make_token(scanner, (token))
		case ';': MAKE_SINGLE_CHAR_TOKEN(TOKEN_SEMICOLON);
		case '?': MAKE_SINGLE_CHAR_TOKEN(TOKEN_QUESTION_MARK);
		case '.': MAKE_SINGLE_CHAR_TOKEN(TOKEN_DOT);
		case ',': MAKE_SINGLE_CHAR_TOKEN(TOKEN_COMMA);
		case '+': MAKE_SINGLE_CHAR_TOKEN(TOKEN_PLUS);
		case '-': MAKE_SINGLE_CHAR_TOKEN(TOKEN_MINUS);
		case '*': MAKE_SINGLE_CHAR_TOKEN(TOKEN_STAR);
		case '/': MAKE_SINGLE_CHAR_TOKEN(TOKEN_SLASH);
		case '(': MAKE_SINGLE_CHAR_TOKEN(TOKEN_PAREN_LEFT);
		case ')': MAKE_SINGLE_CHAR_TOKEN(TOKEN_PAREN_RIGHT);
		case '{': MAKE_SINGLE_CHAR_TOKEN(TOKEN_BRACE_LEFT);
		case '}': MAKE_SINGLE_CHAR_TOKEN(TOKEN_BRACE_RIGHT);
		case '[': MAKE_SINGLE_CHAR_TOKEN(TOKEN_BRACKET_LEFT);
		case ']': MAKE_SINGLE_CHAR_TOKEN(TOKEN_BRACKET_RIGHT);

		// One or two character tokens
		#define MAKE_MULTI_CHAR_TOKEN(next_char, token_match, token_not_match) \
			return soul__scanner_make_token(scanner, \
				soul__scanner_match_next(scanner, (next_char)) ? (token_match) : (token_not_match))
		case ':': MAKE_MULTI_CHAR_TOKEN(':', TOKEN_DOUBLE_COLON, TOKEN_COLON);
		case '=': MAKE_MULTI_CHAR_TOKEN('=', TOKEN_DOUBLE_EQUAL, TOKEN_EQUAL);
		case '!': MAKE_MULTI_CHAR_TOKEN('=', TOKEN_BANG_EQUAL, TOKEN_BANG);
		case '>': MAKE_MULTI_CHAR_TOKEN('=', TOKEN_GREATER_EQUAL, TOKEN_GREATER);
		case '<': MAKE_MULTI_CHAR_TOKEN('=', TOKEN_LESS_EQUAL, TOKEN_LESS);
	}

	// Unknown token!
	return soul__scanner_make_error_token(scanner, "Unrecognized token!");
}


// ------------------------------------------------------------------------
// @TODO Maybe move this out from here when VM needs similar functions.
#define ORION_GROW_CAPACITY(capacity) ((capacity) < 8 ? 8 : (capacity) * 2)
#define ORION_GROW_ARRAY(type, ptr, capacity) \
	(type*)realloc((ptr), (capacity) * sizeof(type))
#define ORION_FREE_ARRAY(type, ptr, capacity) \
	realloc((ptr), 0)
// ------------------------------------------------------------------------

SOUL_API soul_token_array_t soul_scan(const char* buffer, size_t size)
{
	if(size < 1) return soul__invalid_token_array;

	// @TODO: Maybe move scanner initialization to a function?
	soul_scanner_t scanner = {
		.start = buffer,
		.current = buffer,
		.line = 1,
	};

	soul_token_array_t tokens;
	tokens.valid = true;
	tokens.size = 0;
	tokens.capacity = ORION_GROW_CAPACITY(0);
	tokens.data = ORION_GROW_ARRAY(soul_token_t, NULL, tokens.capacity);

	for(;;)
	{
		soul_token_t token = soul__scanner_scan_token(&scanner);

		// @TODO This is bad. We should probably make a designated dynamic_array structure to contain this madness.
		if(tokens.size + 1 > tokens.capacity)
		{
			size_t old_capacity = tokens.capacity;
			tokens.capacity = ORION_GROW_CAPACITY(old_capacity);
			tokens.data = ORION_GROW_ARRAY(soul_token_t, tokens.data, tokens.capacity);
		}

		tokens.data[tokens.size++] = token;
		//

		if(token.type == TOKEN_EOF) break;
	}

	return tokens;
}
