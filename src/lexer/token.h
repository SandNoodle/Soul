#ifndef LEXER_TOKEN_H
#define LEXER_TOKEN_H

#include <stdint.h>
#include <string>
#include <array>

namespace soul
{
	enum class error_code : uint32_t;

	enum class token_type : uint8_t
	{
		token_unknown, // Presence of an unknown token should convert into an error
		               // if we cannot resolve it (somehow).

		// Single character tokens
		token_semicolon, token_question_mark,    // ;?
		token_percent, token_caret,              // %^
		token_dot, token_comma,                  // .,
		token_paren_left, token_paren_right,     // ()
		token_brace_left, token_brace_right,     // {}
		token_bracket_left, token_bracket_right, // []

		// One or two character tokens
		token_colon, token_double_colon,         // : ::
		token_equal, token_double_equal,         // = ==
		token_bang, token_bang_equal,            // ! !=
		token_greater, token_greater_equal,      // > >=
		token_less, token_less_equal,            // < <=
		token_plus, token_plus_equal,            // + +=
		token_minus, token_minus_equal,          // - -=
		token_star, token_star_equal,            // * *=
		token_slash, token_slash_equal,          // / /=
		token_ampersand, token_double_ampersand, // & &&
		token_pipe, token_double_pipe,           // | ||

		// Literals
		token_number,            // ex. 123, 3.14
		token_string,            // ex. "test_string", 'c'
		token_identifier,        // ex. my_identifier

		// Keywords
		token_native,                 // Native C function
		token_import,                 // File import
		token_define,                 // Define alias
		token_let, token_mut,         // Variables
		token_if, token_else,         // Flow control
		token_for, token_while,       // Loops
		token_continue, token_break,  // Loop keywords
		token_return, token_fn,       // Function
		token_struct, token_enum,     // Data types
		token_true, token_false,      // Truthness literals

		// Special tokens
		token_error, // Token containing error message.
		token_eof,   // Token signaling End of File.
	};

	struct token
	{
		token_type  type;
		const char* start;
		size_t      size;
		error_code  error_code; // It is used when type == token_type::token_error.
	};

	struct token_keyword
	{
		const char* start;
		size_t      length;
		token_type  type;
	};

	static const std::array<token_keyword, 16> keywords = {
		token_keyword { "native",   6, token_type::token_native },
		token_keyword { "import",   6, token_type::token_import },
		token_keyword { "define",   6, token_type::token_define },
		token_keyword { "fn",       2, token_type::token_fn },
		token_keyword { "let",      3, token_type::token_let },
		token_keyword { "mut",      3, token_type::token_mut },
		token_keyword { "if",       2, token_type::token_if },
		token_keyword { "else",     4, token_type::token_else },
		token_keyword { "for",      3, token_type::token_for },
		token_keyword { "while",    5, token_type::token_while },
		token_keyword { "continue", 8, token_type::token_continue },
		token_keyword { "break",    5, token_type::token_break },
		token_keyword { "struct",   6, token_type::token_struct },
		token_keyword { "enum",     4, token_type::token_enum },
		token_keyword { "true",     4, token_type::token_true },
		token_keyword { "false",    5, token_type::token_false },
	};

	bool is_literal_token(token_type type);
	bool is_assign_token(token_type type);
	bool is_sync_token(token_type type);

} // namespace soul

#endif // LEXER_TOKEN_H
