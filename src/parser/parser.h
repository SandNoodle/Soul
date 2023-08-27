#ifndef PARSER_PARSER_H
#define PARSER_PARSER_H

#include <stdint.h>
#include <vector>
#include <functional>

namespace soul
{
	enum class token_type : uint8_t;
	enum class ast_node_type : uint8_t;
	struct ast_node;
	struct token;
	struct parser;

	enum precedence : uint8_t {
		prec_none = 0,
		prec_assign,         // =
		prec_or,             // ||
		prec_and,            // &&
		prec_equal,          // == !=
		prec_compare,        // < > <= =>
		prec_additive,       // + -
		prec_multiplicative, // * /
		prec_unary,          // ! -
		prec_call,           // @TODO
		prec_primary,
	};

	typedef ast_node* (parser::*prefix_precedence_fn)(void);
	typedef ast_node* (parser::*infix_precedence_fn)(ast_node* val);

	struct precedence_rule {
		precedence precedence;
		prefix_precedence_fn prefix;
		infix_precedence_fn infix;
	};

	class parser
	{
		public:
			parser() = delete;
			parser(const parser&) = delete;
			parser(parser&&) = delete;
			~parser() = delete;

			/**
			 * @TODO
			 *
			 * @param tokens @TODO
			 * @return ast_node* @TODO
			 */
			ast_node* parse(const std::vector<token>& tokens);

		private:
			/**
			 * Creates new AST Node using provided allocator.
			 */
			ast_node* create_node(ast_node_type type);

			ast_node* parse_statement();
			ast_node* parse_define_statement();
			ast_node* parse_variable_declaration_statement();
			ast_node* parse_function_declaration_statement();
			ast_node* parse_if_statement();
			ast_node* parse_for_statement();
			ast_node* parse_while_statement();
			ast_node* parse_body_statement();

			ast_node* parse_expression();
			ast_node* parse_expression_statement();
			ast_node* parse_precedence_expression(precedence prec);
			ast_node* parse_binary_expression(ast_node* lhs);
			ast_node* parse_unary_expression();
			ast_node* parse_literal_expression();
			ast_node* parse_assign_expression_statement();

			precedence_rule get_precedence_rule(token_type type);

			void advance();
			void synchronize();
			bool match(token_type type) const;
			bool match_any(token_type* types, size_t size) const;
			token require(token_type type);
			token peek() const;
			token peek_next() const;
			token peek_prev() const;

		private:
			bool had_panic;
			bool had_error;

			std::vector<token> tokens;
			size_t current_token;
	};
} // namespace soul

#endif // PARSER_PARSER_H
