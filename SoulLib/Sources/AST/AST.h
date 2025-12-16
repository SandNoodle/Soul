#pragma once

#include "Core/Diagnostic.h"
#include "Soul.h"

#include <limits>
#include <vector>

namespace Soul::AST
{
	using SymbolIndex = UInt32;
	using NodeIndex   = UInt32;
	using TypeIndex   = UInt32;

	static constexpr SymbolIndex k_invalid_symbol_index = std::numeric_limits<SymbolIndex>::max();
	static constexpr NodeIndex k_invalid_node_index     = std::numeric_limits<NodeIndex>::max();
	static constexpr TypeIndex k_invalid_type_index     = std::numeric_limits<TypeIndex>::max();

	enum class ASTNodeOperator : UInt8
	{
		OPERATOR_UNKNOWN,

		// --- Assignment ---
		OPERATOR_ASSIGN,
		OPERATOR_ASSIGN_ADD,
		OPERATOR_ASSIGN_SUB,
		OPERATOR_ASSIGN_MUL,
		OPERATOR_ASSIGN_DIV,
		OPERATOR_ASSIGN_MOD,

		// --- Arithmetic ---
		OPERATOR_ARITHMETIC_ADD,
		OPERATOR_ARITHMETIC_SUB,
		OPERATOR_ARITHMETIC_MUL,
		OPERATOR_ARITHMETIC_MOD,
		OPERATOR_ARITHMETIC_INCREMENT,
		OPERATOR_ARITHMETIC_DECREMENT,

		// --- Comparison ---
		OPERATOR_COMPARE_EQUAL,
		OPERATOR_COMPARE_NOT_EQUAL,
		OPERATOR_COMPARE_GREATER,
		OPERATOR_COMPARE_GREATER_EQUAL,
		OPERATOR_COMPARE_LESS,
		OPERATOR_COMPARE_LESS_EQUAL,

		// --- Logical ---
		OPERATOR_LOGICAL_NOT,
		OPERATOR_LOGICAL_AND,
		OPERATOR_LOGICAL_OR,

		// --- Access ---
		OPERATOR_ACCESS_ARRAY_SUBSCRIPT,
	};

	struct BinaryNode
	{
		NodeIndex lhs      = k_invalid_node_index;
		NodeIndex rhs      = k_invalid_node_index;
		ASTNodeOperator op = ASTNodeOperator::OPERATOR_UNKNOWN;
	};

	struct BlockNode
	{
		NodeIndex* statements      = nullptr;
		NodeIndex statements_count = k_invalid_node_index;
	};

	struct BreakNode
	{
	};

	struct CastNode
	{
		NodeIndex expression       = k_invalid_node_index;
		SymbolIndex type_specifier = k_invalid_symbol_index;
	};

	struct ContinueNode
	{
	};

	struct ErrorNode
	{
		Diagnostic::Index diagnostic_index = Diagnostic::k_invalid_index;
	};

	struct ForLoopNode
	{
		NodeIndex* statements      = nullptr;
		NodeIndex statements_count = k_invalid_node_index;
		NodeIndex initialization   = k_invalid_node_index;
		NodeIndex condition        = k_invalid_node_index;
		NodeIndex update           = k_invalid_node_index;
	};

	struct ForeachLoopNode
	{
		NodeIndex* statements      = nullptr;
		NodeIndex statements_count = k_invalid_node_index;
		NodeIndex variable         = k_invalid_node_index;
		NodeIndex expression       = k_invalid_node_index;
	};

	struct FunctionCallNode
	{
		NodeIndex* parameters       = nullptr;
		NodeIndex parameters_count  = k_invalid_node_index;
		SymbolIndex name_identifier = k_invalid_symbol_index;
	};

	struct FunctionDeclarationNode
	{
		NodeIndex* parameters       = nullptr;
		NodeIndex* statements       = nullptr;
		NodeIndex statements_count  = k_invalid_node_index;
		NodeIndex parameters_count  = k_invalid_node_index;
		SymbolIndex name_identifier = k_invalid_symbol_index;
		SymbolIndex type_specifier  = k_invalid_symbol_index;
	};

	struct IfNode
	{
		NodeIndex* then_statements      = nullptr;
		NodeIndex* else_statements      = nullptr;
		NodeIndex then_statements_count = k_invalid_node_index;
		NodeIndex else_statements_count = k_invalid_node_index;
		NodeIndex condition             = k_invalid_node_index;
	};

	struct LiteralNode
	{
		SymbolIndex value = k_invalid_symbol_index;
	};

	struct ReturnNode
	{
		NodeIndex expression = k_invalid_node_index;
	};

	struct StructDeclarationNode
	{
		NodeIndex* parameters       = nullptr;
		NodeIndex parameters_count  = k_invalid_node_index;
		SymbolIndex name_identifier = k_invalid_symbol_index;
	};

	struct UnaryNode
	{
		NodeIndex expression = k_invalid_node_index;
		ASTNodeOperator op   = ASTNodeOperator::OPERATOR_UNKNOWN;
	};

	struct VariableDeclarationNode
	{
		SymbolIndex name_identifier = k_invalid_symbol_index;
		SymbolIndex type_specifier  = k_invalid_symbol_index;
		NodeIndex expression        = k_invalid_node_index;
		Bool8 is_mutable            = false;
	};

	struct WhileLoopNode
	{
		NodeIndex* statements      = nullptr;
		NodeIndex statements_count = k_invalid_node_index;
		NodeIndex expression       = k_invalid_node_index;
	};

	enum class ASTNodeType : UInt8
	{
		NODE_TYPE_UNKNOWN,

		NODE_TYPE_BINARY,
		NODE_TYPE_BLOCK,
		NODE_TYPE_BREAK,
		NODE_TYPE_CAST,
		NODE_TYPE_CONTINUE,
		NODE_TYPE_ERROR,
		NODE_TYPE_FOR_LOOP,
		NODE_TYPE_FOREACH_LOOP,
		NODE_TYPE_FUNCTION_CALL,
		NODE_TYPE_FUNCTION_DECLARATION,
		NODE_TYPE_IF,
		NODE_TYPE_LITERAL,
		NODE_TYPE_RETURN,
		NODE_TYPE_UNARY,
		NODE_TYPE_VARIABLE_DECLARATION,
		NODE_TYPE_WHILE_LOOP,
	};

	struct ASTNode
	{
		union
		{
			BinaryNode as_binary;
			BlockNode as_block;
			BreakNode as_break;
			CastNode as_cast;
			ContinueNode as_continue;
			ErrorNode as_error;
			ForLoopNode as_forloop;
			ForeachLoopNode as_foreach;
			FunctionCallNode as_function_call;
			FunctionDeclarationNode as_function_declaration;
			IfNode as_if;
			LiteralNode as_literal;
			ReturnNode as_return;
			UnaryNode as_unary;
			VariableDeclarationNode as_variable_declaration;
			WhileLoopNode as_while_loop;
		};
		TypeIndex type_index  = k_invalid_type_index;
		ASTNodeType node_type = ASTNodeType::NODE_TYPE_UNKNOWN;
	};
	static_assert(sizeof(ASTNode) <= SOUL_BYTES(64), "ASTNode should fit in a single cache line (64 bytes).");

	struct ASTModule
	{
	};
}  // namespace Soul::AST
