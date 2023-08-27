#include "ast.h"

#include "lexer/token.h"

namespace soul
{
	ast_node_operator to_node_operator(token_type type)
	{
		switch(type)
		{
			case token_type::token_plus: return ast_node_operator::op_add;
			case token_type::token_minus: return ast_node_operator::op_sub;
			case token_type::token_star: return ast_node_operator::op_mul;
			case token_type::token_slash: return ast_node_operator::op_div;
			case token_type::token_percent: return ast_node_operator::op_mod;
			/* case token_type::token_: return ast_node_operator::op_inc; */
			/* case token_type::token_: return ast_node_operator::op_dec; */
			case token_type::token_caret: return ast_node_operator::op_pow;
			case token_type::token_equal: return ast_node_operator::op_equal;
			case token_type::token_bang_equal: return ast_node_operator::op_not_equal;
			case token_type::token_greater: return ast_node_operator::op_greater;
			case token_type::token_greater_equal: return ast_node_operator::op_greater_equal;
			case token_type::token_less: return ast_node_operator::op_less;
			case token_type::token_less_equal: return ast_node_operator::op_less_equal;
			case token_type::token_bang: return ast_node_operator::op_logic_not;
			case token_type::token_double_ampersand: return ast_node_operator::op_logic_and;
			case token_type::token_double_pipe: return ast_node_operator::op_logic_or;
			default:
				return ast_node_operator::op_none;
		}
	}
} // namespace soul
