#include "compiler.h"

#include "soul_config.h"
#include "ast/ast.h"
#include "compiler/chunk.h"
#include "runtime/opcode.h"
#include "util/error_code.h"

#include <assert.h>

namespace soul
{
	error_code compiler::compile(chunk& chunk, const ast_node* root)
	{
		current_chunk = { };
		had_error = false;
		had_panic = false;
		current_depth = 0;

		compile_node(root);
		chunk = current_chunk;

		return error_code::error_none;
	}

	void compiler::compile_node(const ast_node* node)
	{
		switch(node->type)
		{
			// Expressions
			case ast_node_type::ast_expr_assign:         return compile_assign_expression(node);
			case ast_node_type::ast_expr_binary:         return compile_binary_expression(node);
			case ast_node_type::ast_expr_unary:          return compile_unary_expression(node);
			case ast_node_type::ast_expr_var_literal:    return compile_var_literal(node);
			case ast_node_type::ast_expr_bool_literal:   return compile_bool_literal(node);
			case ast_node_type::ast_expr_number_literal: return compile_number_literal(node);
			case ast_node_type::ast_expr_string_literal: return compile_string_literal(node);
			case ast_node_type::ast_expr_stmt:           return compile_expression_statement(node);
			// Statements
			case ast_node_type::ast_stmt_if:            return compile_if_statement(node);
			case ast_node_type::ast_stmt_for:           return compile_for_statement(node);
			case ast_node_type::ast_stmt_foreach:       return compile_foreach_statement(node);
			case ast_node_type::ast_stmt_while:         return compile_while_statement(node);
			case ast_node_type::ast_stmt_block:         return compile_block_statement(node);
			case ast_node_type::ast_stmt_return:        return compile_return_statement(node);
			case ast_node_type::ast_stmt_import:        return compile_import_statement(node);
			case ast_node_type::ast_stmt_variable_decl: return compile_variable_declaration(node);
			case ast_node_type::ast_stmt_function_decl: return compile_function_declaration(node);
			case ast_node_type::ast_stmt_native_decl:   return compile_native_declaration(node);
			case ast_node_type::ast_stmt_define_decl:   return compile_define_declaration(node);
			default:
				// @TODO Propagate error
				break;
		}
	}

	void compiler::compile_assign_expression(const ast_node* node)
	{
		assert(false && "not implemented yet");
	}

	void compiler::compile_binary_expression(const ast_node* node)
	{
		compile_node(node->as.binary_expr.lval);
		compile_node(node->as.binary_expr.rval);
		// @TODO: Opcode based on type. Currently only integers are supported.
		switch(node->as.binary_expr.op)
		{
			case ast_node_operator::op_add: emit_opcode(opcode::op_addi); break;
			case ast_node_operator::op_sub: emit_opcode(opcode::op_subi); break;
			case ast_node_operator::op_mul: emit_opcode(opcode::op_muli); break;
			case ast_node_operator::op_div: emit_opcode(opcode::op_divi); break;
			default:
				assert(false && "not implemented yet");
		}
	}

	void compiler::compile_unary_expression(const ast_node* node)
	{
		compile_node(node->as.unary_expr.val);
		switch(node->as.unary_expr.op)
		{
			default:
				assert(false && "not implemented yet");
		}
	}

	void compiler::compile_var_literal(const ast_node* node)
	{
		assert(false && "not implemented yet");
	}

	void compiler::compile_bool_literal(const ast_node* node)
	{
		emit_opcode(node->as.bool_lit_expr.val
			? opcode::op_push_true
			: opcode::op_push_false);
	}

	void compiler::compile_number_literal(const ast_node* node)
	{
		uint16_t index = 0; // @TODO Add const
		emit_opcode(opcode::op_get_const);
		emit_short(index);
	}

	void compiler::compile_string_literal(const ast_node* node)
	{
		assert(false && "not implemented yet");
	}

	void compiler::compile_expression_statement(const ast_node* node)
	{
		assert(false && "not implemented yet");
	}

	void compiler::compile_if_statement(const ast_node* node)
	{
		// Condition
		compile_node(node->as.if_stmt.condition);

		// Jumping point
		size_t jump_address = emit_opcode(opcode::op_jump_false);
		emit_short(0); // NOTE: Jump address that will be patched in later.

		// Compile main branch
		compile_node(node->as.if_stmt.then_stmt);

		// Patch the main branch jump
		set_jump_address(jump_address, get_current_address());

		// (Optional) Compile else branch
		if(node->as.if_stmt.else_stmt)
		{
			size_t current_address = get_current_address();
			compile_node(node->as.if_stmt.else_stmt);
			set_jump_address(current_address, get_current_address());
		}
	}

	void compiler::compile_for_statement(const ast_node* node)
	{
		assert(false && "not implemented yet");
	}

	void compiler::compile_foreach_statement(const ast_node* node)
	{
		assert(false && "not implemented yet");
	}

	void compiler::compile_while_statement(const ast_node* node)
	{
		assert(false && "not implemented yet");
	}

	void compiler::compile_block_statement(const ast_node* node)
	{
		enter_scope();
		const auto& statements = node->as.block_stmt.stmts;
		for(size_t i = 0; i < statements.size(); ++i)
		{
			compile_node(statements[i]);
		}
		exit_scope();
	}

	void compiler::compile_return_statement(const ast_node* node)
	{
		assert(false && "not implemented yet");
	}

	void compiler::compile_import_statement(const ast_node* node)
	{
		assert(false && "not implemented yet");
	}

	void compiler::compile_variable_declaration(const ast_node* node)
	{
		if(current_depth < 1)
		{
			// @TODO Propagate error - variables in global scope are not allowed.
			return;
		}

		if(local_variables.size() >= SOUL_MAX_LOCAL_VARIABLES)
		{
			// @TODO Propagate error
			return;
		}

		const auto& identifier = node->as.variable_decl_stmt.identifier;

		// @TODO Disallow shadowing variables regardles of scope.

		// Define variable
		auto variable_index = add_local_variable(identifier);

		// NOTE: Soul requires all of its variables to be defined as they are declared.
		//       Therefore we can compile its r-value expression just before we emit OP_SET_LOCAL.
		compile_node(node->as.variable_decl_stmt.val);

		// Declare variable
		emit_opcode(opcode::op_set_local);
		emit_byte(variable_index);
	}

	void compiler::compile_function_declaration(const ast_node* node)
	{
		assert(false && "not implemented yet");
	}

	void compiler::compile_native_declaration(const ast_node* node)
	{
		assert(false && "not implemented yet");
	}

	void compiler::compile_define_declaration(const ast_node* node)
	{
		assert(false && "not implemented yet");
	}

	void compiler::discard_scopes()
	{
		assert(false && "not implemented yet");
	}

	void compiler::enter_scope()
	{
		assert(false && "not implemented yet");
	}

	void compiler::exit_scope()
	{
		assert(false && "not implemented yet");
	}

	size_t compiler::emit_opcode(opcode op)
	{
		assert(false && "not implemented yet");
	}

	void compiler::emit_byte(byte_t b)
	{
		assert(false && "not implemented yet");
	}

	void compiler::emit_short(short_t s)
	{
		assert(false && "not implemented yet");
	}

	void compiler::set_jump_address(size_t address_location, int16_t address)
	{
		assert(false && "not implemented yet");
	}

	size_t compiler::get_current_address() const
	{
		assert(false && "not implemented yet");
	}

	size_t compiler::add_local_variable(const ast_node_identifier& identifer)
	{
		assert(false && "not implemented yet");
	}
} // namespace soul
