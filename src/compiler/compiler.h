#ifndef COMPILER_COMPILER_H
#define COMPILER_COMPILER_H

#include "compiler/chunk.h"

#include <stdint.h>
#include <string>

namespace soul
{
	struct ast_node;
	enum class error_code : uint32_t;
	enum class opcode : uint8_t;
	using ast_node_identifier = std::string;

	class compiler
	{
		private:
			using byte_t = uint8_t;
			using short_t = uint16_t;

		public:
			compiler() = delete;
			compiler(const compiler&) = delete;
			compiler(compiler&&) = delete;
			~compiler() = delete;

			/**
			 * Compiles AST into vm exectuable bytecode.
			 *
			 * @param root Root of an AST.
			 * @param chunk Compiled bytecode chunk.
			 * @return error_code Status of a compilation.
			 */
			error_code compile(chunk& chunk, const ast_node* root);

		private:
			void compile_node(const ast_node* node);
			void compile_assign_expression(const ast_node* node);
			void compile_binary_expression(const ast_node* node);
			void compile_unary_expression(const ast_node* node);
			void compile_var_literal(const ast_node* node);
			void compile_bool_literal(const ast_node* node);
			void compile_number_literal(const ast_node* node);
			void compile_string_literal(const ast_node* node);
			void compile_expression_statement(const ast_node* node);
			void compile_if_statement(const ast_node* node);
			void compile_for_statement(const ast_node* node);
			void compile_foreach_statement(const ast_node* node);
			void compile_while_statement(const ast_node* node);
			void compile_block_statement(const ast_node* node);
			void compile_return_statement(const ast_node* node);
			void compile_import_statement(const ast_node* node);
			void compile_variable_declaration(const ast_node* node);
			void compile_function_declaration(const ast_node* node);
			void compile_native_declaration(const ast_node* node);
			void compile_define_declaration(const ast_node* node);

			void discard_scopes();
			void enter_scope();
			void exit_scope();

			size_t emit_opcode(opcode op);
			void   emit_byte(byte_t b);
			void   emit_short(short_t s);

			void set_jump_address(size_t address_location, int16_t address);
			size_t get_current_address() const;

			size_t add_local_variable(const ast_node_identifier& identifer);

		private:
			chunk& current_chunk;
			bool had_panic;
			bool had_error;
			size_t current_depth;

			std::vector<bool> local_variables;
	};
} // namespace soul

#endif // COMPILER_COMPILER_H
