#ifndef LEXER_LEXER_H
#define LEXER_LEXER_H

#include <stdint.h>
#include <string>
#include <vector>

namespace soul
{
	enum class error_code : uint32_t;
	enum class token_type : uint8_t;
	struct token;

	class lexer
	{
		public:
			lexer() = delete;
			lexer(const lexer&) = delete;
			lexer(lexer&&) = delete;
			~lexer() = delete;

			/**
			 * @TODO
			 *
			 * @param buffer @TODO
			 * @param size @TODO
			 * @return std::vector<token> @TODO
			 */
			std::vector<token> scan(const char* buffer, size_t size);

		private:
			token scan_token();

			token create_token(token_type type);
			token create_identifier_token();
			token create_number_token();
			token create_string_token();
			token create_error_token(error_code error);

			void skip_whitespace();
			char advance();
			char peek() const;
			bool match_next(char expected);

			bool is_digit(char c);
			bool is_alpha(char c);
			bool is_eof(char c);

		private:
			const char* token_start;
			const char* current_char;
			size_t      current_line;
	};
} // namespace soul

#endif // LEXER_LEXER_H
