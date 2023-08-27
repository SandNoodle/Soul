#include "parser.h"

#include "ast/ast.h"
#include "lexer/token.h"
#include "util/error_code.h"

namespace soul
{
	ast_node* parser::parse(const std::vector<token>& t)
	{
		tokens = t;
		had_panic = false;
		had_error = false;
		current_token = 0;

		std::vector<ast_node*> statements;
		while(tokens[current_token].type != token_type::token_eof)
		{
			statements.emplace_back(parse_statement());
			if(had_panic) synchronize();
		}

		// @TODO Switch to function scope.
		ast_node* node = create_node(ast_node_type::ast_stmt_block);
		node->as.block_stmt.stmts = std::move(statements);
		return node;
	}

	ast_node* parser::create_node(ast_node_type type)
	{
		// @TODO Remove malloc - replace with user provided allocator.
		ast_node* node = (ast_node*)malloc(sizeof(ast_node));
		node->type = type;
		return node;
	}

	ast_node* parser::parse_statement()
	{
		switch(peek().type)
		{
			case token_type::token_define:
				{
					ast_node* s = parse_define_statement();
					require(token_type::token_semicolon);
					return s;
				}
			case token_type::token_let:
				{
					ast_node* s = parse_variable_declaration_statement();
					require(token_type::token_semicolon);
					return s;
				}
			case token_type::token_fn:         return parse_function_declaration_statement();
			case token_type::token_if:         return parse_if_statement();
			case token_type::token_for:        return parse_for_statement();
			case token_type::token_while:      return parse_while_statement();
			case token_type::token_brace_left: return parse_body_statement();
			default:
				break;
		}

		// Expression statements.
		ast_node* statement = parse_expression_statement();
		require(token_type::token_semicolon);
		return statement;
	}

	ast_node* parser::parse_define_statement()
	{
		require(token_type::token_define); // Consume define declaration

		// Identifier
		token identifier_token = require(token_type::token_identifier);
		ast_node_identifier identifier = std::string(identifier_token.start, identifier_token.size);

		// Expression
		ast_node* expr = parse_expression();

		ast_node* node = create_node(ast_node_type::ast_stmt_define_decl);
		node->as.define_decl_stmt.id = identifier;
		node->as.define_decl_stmt.expr = expr;
		return node;
	}

	ast_node* parser::parse_variable_declaration_statement()
	{
		require(token_type::token_let); // Consume variable declaration token

		// (Optional) Mutability
		bool is_mutable = false;
		if(peek().type == token_type::token_mut)
		{
			is_mutable = true;
			advance();
		}

		// Identifier
		token identifier_token = require(token_type::token_identifier);
		ast_node_identifier identifier = std::string(identifier_token.start, identifier_token.size);

		require(token_type::token_colon); // Consume type declaration

		// Token type

		require(token_type::token_equal); // Consume assignment operator

		// Expression
		ast_node* expr = parse_expression();

		ast_node* node = create_node(ast_node_type::ast_stmt_variable_decl);
		node->as.variable_decl_stmt.identifier = std::move(identifier);
		node->as.variable_decl_stmt.val = expr;
		return node;
	}

	ast_node* parser::parse_function_declaration_statement()
	{
		require(token_type::token_fn); // Consume function declaration token

		// Identifier
		token identifier_token = require(token_type::token_identifier);
		ast_node_identifier identifier = std::string(identifier_token.start, identifier_token.size);

		// (Optional) parameters
		// @TODO Skipped for now.
		if(match(token_type::token_paren_left))
		{
			advance(); // Consume parametrization start token.

			while(match(token_type::token_identifier))
			{
				advance(); // @TODO Identifier
				require(token_type::token_colon);
				advance(); // @TODO Type

				if(peek().type != token_type::token_comma) break;
			}

			require(token_type::token_paren_right); // Consume parametrization stop token;
		}

		require(token_type::token_double_colon); // Consume type declaration token

		// Return type
		// @TODO
		token type_token = require(token_type::token_identifier);
		ast_node_identifier return_type = std::string(type_token.start, type_token.size);

		// Function body
		ast_node* body = parse_body_statement();

		ast_node* node = create_node(ast_node_type::ast_stmt_function_decl);
		node->as.function_decl_stmt.identifier = identifier;
		node->as.function_decl_stmt.body = body;
		return node;
	}

	ast_node* parser::parse_if_statement()
	{
		require(token_type::token_if); // Consume if statement token

		// Condition
		require(token_type::token_paren_left);
		ast_node* condition = parse_expression();
		require(token_type::token_paren_right);

		// Then block
		ast_node* then_stmt = parse_body_statement();

		// (Optional) Else block
		ast_node* else_stmt = nullptr;
		if(match(token_type::token_else))
		{
			advance(); // Consume else token
			else_stmt = parse_body_statement();
		}

		ast_node* node = create_node(ast_node_type::ast_stmt_if);
		node->as.if_stmt.condition = condition;
		node->as.if_stmt.then_stmt = then_stmt;
		node->as.if_stmt.else_stmt = else_stmt;

		return node;
	}

	ast_node* parser::parse_for_statement()
	{
		require(token_type::token_for); // Consumre for statement token
		require(token_type::token_paren_left);

		// (Optional) Mutable variable declaration
		ast_node* init = nullptr;
		if(!match(token_type::token_semicolon))
		{
			init = parse_variable_declaration_statement();
		}

		// (Optional) Condition
		ast_node* condition = nullptr;
		if(!match(token_type::token_semicolon))
		{
			condition = parse_expression();
		}

		// (Optional) Iteration expression
		ast_node* expr = nullptr;
		if(!match(token_type::token_paren_right))
		{
			expr = parse_expression();
		}

		require(token_type::token_paren_right);

		// Body
		ast_node* body = parse_body_statement();

		ast_node* node = create_node(ast_node_type::ast_stmt_for);
		node->as.for_stmt.initializer = init;
		node->as.for_stmt.condition = condition;
		node->as.for_stmt.expr = expr;
		node->as.for_stmt.body = body;
		return node;
	}

	ast_node* parser::parse_while_statement()
	{
		require(token_type::token_while); // Consume while statement token

		ast_node* condition = nullptr;
		bool has_params = match(token_type::token_paren_left);
		if(has_params)
		{
			advance();
			condition = parse_expression();
			require(token_type::token_paren_right);
		}
		else
		{
			// NOTE: Parameterless variant has form: while { <body > }
			condition = parse_expression();
			advance();
		}

		// Body
		ast_node* body = parse_body_statement();

		ast_node* node = create_node(ast_node_type::ast_stmt_while);
		node->as.while_stmt.condition = condition;
		node->as.while_stmt.body = body;
		return node;
	}

	ast_node* parser::parse_body_statement()
	{
		require(token_type::token_brace_left); // Consume block start token

		std::vector<ast_node*> statements;
		while(peek().type != token_type::token_brace_right)
		{
			statements.emplace_back(parse_statement());
			// NOTE: Prevent trying to parse unterminated blocks.
			if(peek().type == token_type::token_eof) break;
		}

		require(token_type::token_brace_right); // Consume block end token

		ast_node* node = create_node(ast_node_type::ast_stmt_block);
		node->as.block_stmt.stmts = std::move(statements);
		return node;
	}

	ast_node* parser::parse_expression()
	{
		// NOTE: Starting precedence has to be at least 1 higher than no precedence.
		return parse_precedence_expression((precedence)(precedence::prec_none + 1));
	}

	ast_node* parser::parse_expression_statement()
	{
		token_type next_type = peek_next().type;

		if(is_assign_token(next_type))
			return parse_assign_expression_statement();

		ast_node* expr = parse_expression();
		expr->type = ast_node_type::ast_expr_stmt;
		expr->as.expr_stmt.expr = expr;
		return expr;
	}

	ast_node* parser::parse_precedence_expression(precedence prec)
	{
		advance(); // Skip op token.

		prefix_precedence_fn prefix_rule = get_precedence_rule(peek_prev().type).prefix;
		if(!prefix_rule)
		{
			// @TODO Propate error: expected expression
			return nullptr;
		}
		ast_node* prefix_expr = (this->*prefix_rule)();

		while(prec <= get_precedence_rule(tokens[current_token].type).precedence)
		{
			advance();
			infix_precedence_fn infix_rule = get_precedence_rule(peek_prev().type).infix;
			prefix_expr = (this->*infix_rule)(prefix_expr);
		}

		return prefix_expr;
	}

	ast_node* parser::parse_binary_expression(ast_node* lhs)
	{
		token op = peek_prev();
		precedence_rule rule = get_precedence_rule(op.type);
		ast_node* rhs = parse_precedence_expression((precedence)(rule.precedence + 1));
		ast_node* node = create_node(ast_node_type::ast_expr_binary);
		node->as.binary_expr.op = to_node_operator(op.type);
		node->as.binary_expr.lval = lhs;
		node->as.binary_expr.rval = rhs;
		return node;
	}

	ast_node* parser::parse_unary_expression()
	{
		token op = peek_prev();
		advance();
		ast_node* val = parse_literal_expression();
		ast_node* node = create_node(ast_node_type::ast_expr_unary);
		node->as.unary_expr.op = to_node_operator(op.type);
		node->as.unary_expr.val = val;
		return node;
	}

	ast_node* parser::parse_literal_expression()
	{
		ast_node* node = nullptr;
		token t = peek_prev();
		switch(t.type)
		{
			case token_type::token_identifier:
				{
					node->as.var_lit_expr.val = std::string(t.start, t.size);
					return node;
				}
			case token_type::token_number:
				{
					// @TODO Parsing all number types.
					//       Currently we asume that was can parse only integers,
					//       but Soul supports real numbers too. Figure a way to parse both of them.
					node->as.number_lit_expr.type = ast_node_number_literal::number_type::integer;
					node->as.number_lit_expr.as.int_val  = (int64_t)strtoll(t.start, nullptr, 0);
					return node;
				}
			case token_type::token_string:
				{
					node->as.string_lit_expr.val = std::string(t.start + 1, t.size - 2);
					return node;
				}
			case token_type::token_true:
			case token_type::token_false:
				{
					node->as.bool_lit_expr.val = t.type != token_type::token_false;
					return node;
				}
			default:
			   break;
		}

		// @TODO Propagate error. Expected literal.
		return node;
	}

	ast_node* parser::parse_assign_expression_statement()
	{
		ast_node* lhs = parse_expression();
		require(token_type::token_equal); // Consume assigment operator
		ast_node* rhs = parse_expression();

		ast_node* expr = create_node(ast_node_type::ast_expr_assign);
		expr->as.assign_expr.lhs = lhs;
		expr->as.assign_expr.lhs = rhs;
		return expr;
	}

	precedence_rule parser::get_precedence_rule(token_type type)
	{
		if(is_literal_token(type))
			return (precedence_rule) { precedence::prec_none, &parser::parse_literal_expression, nullptr };

		switch(type)
		{
			case token_type::token_minus:
				return (precedence_rule)
					{
						precedence::prec_additive,
						&parser::parse_unary_expression,
						&parser::parse_binary_expression,
					};
			case token_type::token_plus:
				return (precedence_rule)
					{
						precedence::prec_additive,
						nullptr,
						&parser::parse_binary_expression,
					};
			case token_type::token_star:
			case token_type::token_slash:
				return (precedence_rule)
					{
						precedence::prec_multiplicative,
						nullptr,
						&parser::parse_binary_expression,
					};
			default:
				// NOTE: No precedence
				return (precedence_rule) { precedence::prec_none, nullptr, nullptr };
		}
	}

	void parser::advance()
	{
		if(current_token + 1 >= tokens.size())
		{
			// @TODO Propagate error.
			return;
		}

		current_token++;
		if(tokens[current_token].type == token_type::token_error)
		{
			// @TODO Propagate error.
		}
	}

	void parser::synchronize()
	{
		had_panic = false;

		while(!match(token_type::token_eof))
		{
			if(!is_sync_token(tokens[current_token].type))
				advance();

			break; // Synchronized!
		}
	}

	bool parser::match(token_type type) const
	{
		return tokens[current_token].type == type;
	}

	bool parser::match_any(token_type* types, size_t size) const
	{
		for(size_t i = 0; i < size; ++i)
		{
			if(match(types[i])) return true;
		}
		return false;
	}

	token parser::require(token_type type)
	{
		if(peek().type != type)
		{
			// @TODO Propagate error.
			// @TODO Refactor as "error_token" fn from token.h
			return token {
				.type = token_type::token_error,
				.start = nullptr,
				.size = 0,
				.error_code = error_code::error_parser_unexpected_token,
			};
		}

		advance();
		return peek_prev();
	}

	token parser::peek() const
	{
		return tokens.at(current_token);
	}

	token parser::peek_next() const
	{
		return tokens.at(current_token + 1);
	}

	token parser::peek_prev() const
	{
		return tokens.at(current_token - 1);
	}
} // namespace soul
