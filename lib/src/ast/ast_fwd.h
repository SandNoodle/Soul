#pragma once

#include <concepts>
#include <type_traits>

namespace soul::ast
{
#define SOUL_AST_NODES                     \
	SOUL_AST_NODE(BinaryNode)              \
	SOUL_AST_NODE(BlockNode)               \
	SOUL_AST_NODE(CastNode)                \
	SOUL_AST_NODE(ErrorNode)               \
	SOUL_AST_NODE(ForLoopNode)             \
	SOUL_AST_NODE(ForeachLoopNode)         \
	SOUL_AST_NODE(FunctionCallNode)        \
	SOUL_AST_NODE(FunctionDeclarationNode) \
	SOUL_AST_NODE(IfNode)                  \
	SOUL_AST_NODE(LiteralNode)             \
	SOUL_AST_NODE(LoopControlNode)         \
	SOUL_AST_NODE(ModuleNode)              \
	SOUL_AST_NODE(ReturnNode)              \
	SOUL_AST_NODE(StructDeclarationNode)   \
	SOUL_AST_NODE(UnaryNode)               \
	SOUL_AST_NODE(VariableDeclarationNode) \
	SOUL_AST_NODE(WhileNode)

	// NOLINTNEXTLINE(bugprone-macro-parentheses)
#define SOUL_AST_NODE(name) class name;
	SOUL_AST_NODES
#undef SOUL_AST_NODE

	/** @brief NodeKind is a concept that specifies nodes that can be represented in the Abstract Syntax Tree (AST). */
	template <typename T>
	concept NodeKind = std::same_as<T, BinaryNode>               //
	                || std::same_as<T, BlockNode>                //
	                || std::same_as<T, CastNode>                 //
	                || std::same_as<T, ErrorNode>                //
	                || std::same_as<T, ForLoopNode>              //
	                || std::same_as<T, ForeachLoopNode>          //
	                || std::same_as<T, FunctionCallNode>         //
	                || std::same_as<T, FunctionDeclarationNode>  //
	                || std::same_as<T, IfNode>                   //
	                || std::same_as<T, LiteralNode>              //
	                || std::same_as<T, LoopControlNode>          //
	                || std::same_as<T, ModuleNode>               //
	                || std::same_as<T, ReturnNode>               //
	                || std::same_as<T, StructDeclarationNode>    //
	                || std::same_as<T, UnaryNode>                //
	                || std::same_as<T, VariableDeclarationNode>  //
	                || std::same_as<T, WhileNode>                //
		;
}  // namespace soul::ast
