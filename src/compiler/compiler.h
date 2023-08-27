#ifndef COMPILER_COMPILER_H
#define COMPILER_COMPILER_H

namespace soul
{
	struct ast_node;
	struct chunk;

	class compiler
	{
		public:
			compiler() = delete;
			compiler(const compiler&) = delete;
			compiler(compiler&&) = delete;
			~compiler() = delete;

			/**
			 * Compiles AST into vm exectuable bytecode.
			 *
			 * @param root Root of an AST.
			 * @return chunk Compiled bytecode.
			 */
			chunk compile(const ast_node* root);

		private:
	};
} // namespace soul

#endif // COMPILER_COMPILER_H
