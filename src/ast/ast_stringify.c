#include "ast_stringify.h"

#include "ast/ast.h"

#include <stdio.h>

// TODO: Make it readable. D:

static void print_indent(size_t level)
{
	for (size_t i = 0; i < level; ++i)
	{
		printf("  ");
	}
}

static void print_identifier(soul_ast_node_identifier_t* identifier)
{
	printf("[%.*s]", (int)identifier->length, identifier->data);
}

static void stringify_impl(soul_ast_node_t* node, size_t indent)
{
	if(!node) return;

	print_indent(indent);
	printf("[%s]\n", soul_ast_node_type_to_string(node->type));
	print_indent(indent);
	switch(node->type)
	{
		case soul_ast_expr_assign: {
			struct soul_ast_expr_assign_t* e = &node->as.expr_assign;
			stringify_impl(e->lhs, indent++);
			stringify_impl(e->rhs, indent++);
			break;
	    }
		case soul_ast_expr_unary: {
			struct soul_ast_expr_unary_t* e = &node->as.expr_unary;
			printf("[%s]\n", soul_token_type_to_operator(e->op));
			print_indent(indent);
			stringify_impl(e->expr, indent++);
			break;
		}
		case soul_ast_expr_binary: {
			struct soul_ast_expr_binary_t* e = &node->as.expr_binary;
			printf("[%s]\n", soul_token_type_to_operator(e->op));
			print_indent(indent);
			stringify_impl(e->lhs, indent++);
			stringify_impl(e->rhs, indent++);
			break;
		}
		case soul_ast_expr_var_literal: {
			struct soul_ast_expr_literal_variable_t* e = &node->as.expr_literal_variable;
			print_identifier(&e->id);
			printf("\n");
			break;
		}
		case soul_ast_expr_bool_literal: {
			struct soul_ast_expr_literal_bool_t* e = &node->as.expr_literal_bool;
			printf("[%s]", e->val ? "true" : "false");
			printf("\n");
			break;
		}
		case soul_ast_expr_number_literal: {
			struct soul_ast_expr_literal_number_t* e = &node->as.expr_literal_number;
			printf("[%d]", e->val);
			printf("\n");
			break;
		}
		case soul_ast_expr_string_literal: {
			struct soul_ast_expr_literal_string_t* e = &node->as.expr_literal_string;
			print_identifier(&e->val);
			printf("\n");
			break;
		}
		case soul_ast_expr_stmt: {
			stringify_impl(node->as.expr_stmt.stmt, indent);
			break;
		}
		case soul_ast_stmt_variable_decl: {
			struct soul_ast_stmt_variable_decl_t* e = &node->as.stmt_variable_decl;
			print_indent(indent++);
			print_identifier(&e->id);
			printf(" ( %s ) : ", e->is_mutable ? "mutable" : "const");
			print_identifier(&e->type);
			printf("\n");
			stringify_impl(e->expr, indent);
			indent--;
			break;
		}
		case soul_ast_stmt_function_decl: {
			break;
		}
		case soul_ast_stmt_native_decl: {
			break;
		}
		case soul_ast_stmt_if: {
			struct soul_ast_stmt_if_t* e = &node->as.stmt_if;
			stringify_impl(e->condition, indent++);
			stringify_impl(e->then_body, indent++);
			stringify_impl(e->else_body, indent++);
			break;
		}
		case soul_ast_stmt_for: {
			struct soul_ast_stmt_for_t* e = &node->as.stmt_for;
			stringify_impl(e->initializer, indent++);
			stringify_impl(e->condition, indent++);
			stringify_impl(e->increment_stmt, indent++);
			stringify_impl(e->body, indent++);
			break;
		}
		case soul_ast_stmt_while: {
			struct soul_ast_stmt_while_t* e = &node->as.stmt_while;
			stringify_impl(e->condition, indent++);
			stringify_impl(e->body, indent++);
			break;
		}
		case soul_ast_stmt_block: {
			struct soul_ast_stmt_block_t* e = &node->as.stmt_block;
			for (size_t i = 0; i < e->stmts.size; ++i)
			{
				stringify_impl(e->stmts.nodes[i], indent++);
			}
			break;
		}
		case soul_ast_stmt_return: {
			struct soul_ast_stmt_return_t* e = &node->as.stmt_return;
			stringify_impl(e->expr, indent++);
			break;
		}
		default:
			printf("[__ERROR__] Unhandled soul_ast_node_t in soul_ast_node_stringfy(soul_ast_node_t*)\n");
			break;
	}

}

void soul_ast_node_stringify(soul_ast_node_t* node)
{
	size_t indent = 0;
	stringify_impl(node, indent);
}
