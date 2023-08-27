#ifndef ERROR_CODE_H
#define ERROR_CODE_H

#include <stdint.h>

#include <string>
#include <unordered_map>

namespace soul
{
	enum class error_code : uint32_t
	{
		error_unknown, // This error should be used if no error code exists,
		               // in which case it should be created ASAP!

		// Lexer errors
		error_lexer_unrecognized_token,
		error_lexer_unterminated_string,

		// Parser errors
		error_parser_invalid_token,
		error_parser_unexpected_token,

		// Compiler errors
		error_compiler_not_defined_variable,

		// Runtime errors
		error_runtime_unknown_opcode,
	};

	static const std::unordered_map<error_code, std::string> error_to_string = {
		{ error_code::error_unknown, "Unknown error" },

		// Lexer errors
		{ error_code::error_lexer_unrecognized_token, "unrecognized token" },
		{ error_code::error_lexer_unterminated_string, "unterminated string" },

		// Parser errors
		{ error_code::error_parser_invalid_token, "invalid token" },
		{ error_code::error_parser_unexpected_token, "unexcpected token" },

		// Compiler errors
		{ error_code::error_compiler_not_defined_variable, "undefined variable" },

		// Runtime errors
		{ error_code::error_runtime_unknown_opcode, "unknown opcode" },
	};
} // namespace soul

#endif // ERROR_CODE_H
