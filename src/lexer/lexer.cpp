#include "lexer.h"

#include "lexer/token.h"
#include "util/error_code.h"

#include <string.h>

namespace soul
{
	std::vector<token> lexer::scan(const char* buffer, size_t size)
	{
		token_start = buffer;
		current_char = buffer;
		current_line = 0;

		std::vector<token> tokens;
		for(;;)
		{
			tokens.emplace_back(scan_token());
			if(tokens.back().type == token_type::token_eof) break;
		}

		return tokens;
	}

	token lexer::scan_token()
	{
		skip_whitespace();

		char c = advance();

		if(is_eof(c)) return create_token(token_type::token_eof);

		if(is_alpha(c)) return create_identifier_token();
		if(is_digit(c)) return create_number_token();

		switch(c)
		{
			// String literals
			case '"': return create_string_token();

			// Single character tokens
			#define MAKE_SINGLE_CHAR_TOKEN(type) return create_token((type))
			case ';': MAKE_SINGLE_CHAR_TOKEN(token_type::token_semicolon);
			case '?': MAKE_SINGLE_CHAR_TOKEN(token_type::token_question_mark);
			case '%': MAKE_SINGLE_CHAR_TOKEN(token_type::token_percent);
			case '.': MAKE_SINGLE_CHAR_TOKEN(token_type::token_dot);
			case ',': MAKE_SINGLE_CHAR_TOKEN(token_type::token_comma);
			case '(': MAKE_SINGLE_CHAR_TOKEN(token_type::token_paren_left);
			case ')': MAKE_SINGLE_CHAR_TOKEN(token_type::token_paren_right);
			case '{': MAKE_SINGLE_CHAR_TOKEN(token_type::token_brace_left);
			case '}': MAKE_SINGLE_CHAR_TOKEN(token_type::token_brace_right);
			case '[': MAKE_SINGLE_CHAR_TOKEN(token_type::token_bracket_left);
			case ']': MAKE_SINGLE_CHAR_TOKEN(token_type::token_bracket_right);

			// One or two character tokens
			#define MAKE_MULTI_CHAR_TOKEN(next_char, type_match, type_not_match) \
				return create_token(match_next((next_char)) ? (type_match) : (type_not_match))
			case ':': MAKE_MULTI_CHAR_TOKEN(':', token_type::token_double_colon,     token_type::token_colon);
			case '=': MAKE_MULTI_CHAR_TOKEN('=', token_type::token_double_equal,     token_type::token_equal);
			case '!': MAKE_MULTI_CHAR_TOKEN('=', token_type::token_bang_equal,       token_type::token_bang);
			case '>': MAKE_MULTI_CHAR_TOKEN('=', token_type::token_greater_equal,    token_type::token_greater);
			case '<': MAKE_MULTI_CHAR_TOKEN('=', token_type::token_less_equal,       token_type::token_less);
			case '+': MAKE_MULTI_CHAR_TOKEN('=', token_type::token_plus_equal,       token_type::token_plus);
			case '-': MAKE_MULTI_CHAR_TOKEN('=', token_type::token_minus_equal,      token_type::token_minus);
			case '*': MAKE_MULTI_CHAR_TOKEN('=', token_type::token_star_equal,       token_type::token_star);
			case '/': MAKE_MULTI_CHAR_TOKEN('=', token_type::token_slash_equal,      token_type::token_slash);
			case '&': MAKE_MULTI_CHAR_TOKEN('&', token_type::token_double_ampersand, token_type::token_ampersand);
			case '|': MAKE_MULTI_CHAR_TOKEN('|', token_type::token_double_pipe,      token_type::token_pipe);
		}

		return create_error_token(error_code::error_lexer_unrecognized_token);
	}

	token lexer::create_token(token_type type)
	{
		return token {
			.type = type,
			.start = token_start,
			.size = (size_t)(current_char - token_start),
			.error_code = error_code::error_unknown,
		};
	}

	token lexer::create_identifier_token()
	{
		while(is_alpha(peek()) || is_digit(peek()))
			advance();

		// Check for keywords
		const size_t length = current_char - token_start;
		for(size_t index = 0; index < keywords.size(); ++index)
		{
			const token_keyword keyword = keywords[index];
			const bool same_size = length == keyword.length;
			const bool same_string = memcmp(token_start, keyword.start, length) == 0;
			if(same_size && same_string)
				return create_token(keyword.type);
		}

		return create_token(token_type::token_identifier);
	}

	// @TODO This function currently supports only parsing integers.
	//       Add support for parsing: floating point numbers (IEE754), binary, hex.
	token lexer::create_number_token()
	{
		while(is_digit(peek()))
			advance();

		return create_token(token_type::token_number);
	}

	token lexer::create_string_token()
	{
		while(peek() != '"' && !is_eof(peek()))
		{
			if(peek() == '\n')
				current_line++;

			advance();
		}

		if(is_eof(peek()))
			return create_error_token(error_code::error_lexer_unterminated_string);

		advance();
		return create_token(token_type::token_string);
	}

	token lexer::create_error_token(error_code error)
	{
		return token {
			.type = token_type::token_error,
			.start = nullptr,
			.size = 0,
			.error_code = error,
		};
	}

	void lexer::skip_whitespace()
	{
		for(;;)
		{
			char c = peek();
			switch(c)
			{
				case ' ':
				case '\r':
				case '\t':
					advance();
					break;
				case '\n':
					advance();
					current_line++;
					break;
				case '#': // Comments
					advance(); // remove '#'
					while( peek() != '\n'
						&& peek() != '\r'
						&& !is_eof(peek()))
					{
						advance();
					}
					break;
				default:
					return;
			}
		}
	}

	char lexer::advance()
	{
		return *(current_char++);
	}

	char lexer::peek() const
	{
		return *current_char;
	}

	bool lexer::match_next(char expected)
	{
		char next = peek();
		if(is_eof(next) || next != expected)
			return false;

		current_char++;
		return true;
	}

	bool lexer::is_digit(char c)
	{
		return (c >= '0' && c <= '9');
	}

	bool lexer::is_alpha(char c)
	{
		return (c >= 'a' && c <= 'z')
			|| (c >= 'A' && c <= 'Z')
			||  c == '_';
	}

	bool lexer::is_eof(char c)
	{
		return c == '\0';
	}
} // namespace soul
