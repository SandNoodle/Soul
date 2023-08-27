#include "token.h"

namespace soul
{
	bool is_literal(token_type type)
	{
		return type == token_type::token_identifier
			|| type == token_type::token_number
			|| type == token_type::token_true
			|| type == token_type::token_false
			|| type == token_type::token_string;
	}

	bool is_assign(token_type type)
	{
		return type == token_type::token_equal
			|| type == token_type::token_plus_equal
			|| type == token_type::token_minus_equal
			|| type == token_type::token_star_equal
			|| type == token_type::token_slash_equal;
	}

	bool is_sync_token(token_type type)
	{
		return type == token_type::token_fn
			|| type == token_type::token_let
			|| type == token_type::token_if
			|| type == token_type::token_for
			|| type == token_type::token_while
			|| type == token_type::token_return
			|| type == token_type::token_struct
			|| type == token_type::token_enum
			|| type == token_type::token_brace_left; // Blocks
	}
} // namespace soul
