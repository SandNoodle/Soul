#pragma once

#include "ast/ast_fwd.h"
#include "ast/visitors/visitor.h"
#include "types/type.h"
#include "common/value.h"
#include "core/types.h"
#include "lexer/token.h"
#include "parser/type_specifier.h"

#include <memory>
#include <string>
#include <type_traits>
#include <vector>

namespace soul::ast
{
	/**
	 * @brief IVisitable interface enables visitor traversal of a given class.
	 */
	class IVisitable
	{
		public:
		virtual ~IVisitable() = default;

		virtual void accept(visitors::IVisitor& visitor)       = 0;
		virtual void accept(visitors::IVisitor& visitor) const = 0;

		friend visitors::IVisitor;
	};

	/**
	 * @brief Represents a single Node in the Abstract Syntax Tree (AST).
	 */
	class ASTNode : public IVisitable
	{
		public:
		using Dependency   = std::unique_ptr<ASTNode>;
		using Dependencies = std::vector<Dependency>;
		using Reference    = ASTNode*;
		using Identifier   = std::string;
		using ScopeBlock   = Dependency;
		enum class Operator : u8;

		public:
		types::Type type = {};

		public:
		virtual ~ASTNode() = default;

		/** @brief Verifies if node is of a given type. */
		template <NodeKind Node>
		constexpr bool is() const noexcept
		{
			return dynamic_cast<const Node*>(this) != nullptr;
		}

		/**
		 * @brief Returns the underlying node.
		 * @important Does not perform any validation - assumes that ASTNode::is<T> was used first.
		 */
		template <NodeKind Node>
		constexpr Node& as() noexcept
		{
			return dynamic_cast<Node&>(*this);
		}

		static std::string_view name(const Operator op) noexcept;
		static std::string_view internal_name(const Operator op) noexcept;
		static Operator as_operator(Token::Type) noexcept;
	};

	/**
	 * @brief VisitorAcceptor is a utility class that simplifies traversing the AST, by calling the correct
	 * `Visitor::visit` method for a given node.
	 * @tparam Node Type that satisfies NodeKind concept.
	 */
	template <NodeKind Node>
	class VisitorAcceptor : public ASTNode
	{
		private:
		VisitorAcceptor() = default;
		void accept(visitors::IVisitor& visitor) override { visitor.visit(static_cast<Node&>(*this)); }
		void accept(visitors::IVisitor& visitor) const override { visitor.visit(static_cast<const Node&>(*this)); }

		friend Node;
	};

	/**
	 * @brief Operator which represents the type of binding relation between nodes.
	 */
	enum class ASTNode::Operator : u8
	{
		Unknown,

		// Assignment
		Assign,
		AddAssign,
		SubAssign,
		MulAssign,
		DivAssign,
		ModAssign,

		// Arithmetic
		Add,
		Sub,
		Mul,
		Div,
		Mod,
		Increment,
		Decrement,

		// Comparison
		Equal,
		NotEqual,
		Greater,
		GreaterEqual,
		Less,
		LessEqual,

		// Logical
		LogicalNot,
		LogicalAnd,
		LogicalOr,
	};

	/**
	 * @brief Represents a 'Binary' expression in the Abstract Syntax Tree (AST).
	 * Contains two operands bound by an operator.
	 */
	class BinaryNode final : public VisitorAcceptor<BinaryNode>
	{
		public:
		Operator op    = Operator::Unknown;
		Dependency lhs = nullptr;
		Dependency rhs = nullptr;

		public:
		explicit BinaryNode(Dependency lhs, Dependency rhs, Operator op);
		~BinaryNode() override = default;

		/**
		 * @brief Constructs new Binary expression node.
		 * @param lhs Left-hand side expression.
		 * @param rhs Right-hand side expression.
		 * @param op Operator binding the two sides together.
		 * @return New 'Binary' expression node.
		 */
		static Dependency create(Dependency lhs, Dependency rhs, Operator op);
	};

	/**
	 * @brief Represents an array of statements (in the same scope) in the Abstract Syntax Tree (AST).
	 */
	class BlockNode : public VisitorAcceptor<BlockNode>
	{
		public:
		Dependencies statements = {};

		public:
		explicit BlockNode(Dependencies dependencies);
		~BlockNode() override = default;

		/**
		 * @brief Constructs new Block expression node.
		 * @param statements
		 * @return New 'Block' expression node.
		 */
		static Dependency create(Dependencies statements);
	};

	/**
	 * @brief Represents a 'Cast' expression in the Abstract Syntax Tree (AST).
	 */
	class CastNode : public VisitorAcceptor<CastNode>
	{
		public:
		enum class Type : u8
		{
			Implicit,
			Explicit,
			Impossible,
		};

		public:
		Dependency expression = nullptr;
		parser::TypeSpecifier type_specifier;

		public:
		explicit CastNode(Dependency expression, parser::TypeSpecifier type_specifier);
		~CastNode() override = default;

		/**
		 * @brief Constructs new Cast expression node.
		 * @param expr Expression to cast.
		 * @param type_specifier Type to cast to.
		 * @return New 'Cast' expression node.
		 */
		static Dependency create(Dependency expression, parser::TypeSpecifier type_specifier);
	};

	/**
	 * @brief Represents any invalid state, which prevents further processing in the Abstract Syntax Tree (AST).
	 */
	class ErrorNode final : public VisitorAcceptor<ErrorNode>
	{
		public:
		using Message = std::string;

		public:
		Message message;

		public:
		explicit ErrorNode(Message message);
		~ErrorNode() override = default;

		/**
		 * @brief Constructs new Error node.
		 * @param message Error message associated with this node.
		 */
		static Dependency create(Message message);
	};

	/**
	 * @brief Represents any conditional 'ForLoop' expression statement
	 * in the Abstract Syntax Tree (AST), such as `for` or `while` loops.
	 */
	class ForLoopNode final : public VisitorAcceptor<ForLoopNode>
	{
		public:
		Dependency initialization;
		Dependency condition;
		Dependency update;
		ScopeBlock statements;

		public:
		explicit ForLoopNode(Dependency initialization,
		                     Dependency condition,
		                     Dependency update,
		                     ScopeBlock statements) noexcept;
		~ForLoopNode() override = default;

		/**
		 * @brief Constructs new ForLoop expression statement node.
		 * @param initialization [Optional] Expression executed once before the start of the loop.
		 * @param condition [Optional] Expression verified after each loop iteration.
		 * @param update [Optional] Expression updated after each loop iteration (after condition expression).
		 * @param statements List of statements to execute each loop.
		 * @return New 'ForLoop' expression statement node.
		 */
		static Dependency create(Dependency initialization,
		                         Dependency condition,
		                         Dependency update,
		                         ScopeBlock statements);
	};

	/**
	 * @brief Represents any conditional 'ForeachLoop' expression statement
	 * in the Abstract Syntax Tree (AST).
	 */
	class ForeachLoopNode final : public VisitorAcceptor<ForeachLoopNode>
	{
		public:
		Dependency variable;
		Dependency in_expression;
		ScopeBlock statements;

		public:
		explicit ForeachLoopNode(Dependency variable, Dependency in_expression, ScopeBlock statements) noexcept;
		~ForeachLoopNode() override = default;

		/**
		 * @brief Constructs new ForeachLoop expression statement node.
		 * @param variable Expression representing value of each iteration.
		 * @param in_expression Expression to iterate upon.
		 * @param statements List of statements to execute each loop.
		 * @return New 'ForeachLoop' expression statement node.
		 */
		static Dependency create(Dependency variable, Dependency in_expression, ScopeBlock statements);
	};

	/**
	 * @brief Represents a Function call statement
	 * in the Abstract Syntax Tree (AST).
	 */
	class FunctionCallNode final : public VisitorAcceptor<FunctionCallNode>
	{
		public:
		Identifier name;
		Dependencies parameters;

		public:
		explicit FunctionCallNode(Identifier name, Dependencies parameters);
		~FunctionCallNode() override = default;

		/**
		 * @brief Constructs new FunctionCallNode node.
		 * @param name Name of the function.
		 * @param parameters Parameters this function takes.
		 * @return New 'FunctionCallNode' node.
		 */
		static Dependency create(Identifier name, Dependencies parameters);
	};

	/**
	 * @brief Represents a 'Function Declaration' statement
	 * in the Abstract Syntax Tree (AST).
	 */
	class FunctionDeclarationNode final : public VisitorAcceptor<FunctionDeclarationNode>
	{
		public:
		Identifier name;
		parser::TypeSpecifier type_specifier;
		Dependencies parameters;
		ScopeBlock statements;

		public:
		explicit FunctionDeclarationNode(Identifier identifier,
		                                 parser::TypeSpecifier return_type_specifier,
		                                 Dependencies parameters,
		                                 ScopeBlock statements);
		~FunctionDeclarationNode() override = default;

		/**
		 * @brief Constructs new Function Declaration statement node.
		 * @param name Name of the function.
		 * @param return_type_specifier Type which this function returns.
		 * @param parameters [Optional] Parameters this function takes.
		 * @param statements [Optional] Statements this function executes.
		 * @return New 'Function Declaration' statement node.
		 */
		static Dependency create(Identifier name,
								 parser::TypeSpecifier return_type_specifier,
		                         Dependencies parameters,
		                         ScopeBlock statements);
	};

	/**
	 * @brief Represents an `If` statement in the Abstract Syntax Tree (AST).
	 * Used for expressing conditional expressions.
	 */
	class IfNode final : public VisitorAcceptor<IfNode>
	{
		public:
		Dependency condition;
		ScopeBlock then_statements;
		ScopeBlock else_statements;

		public:
		explicit IfNode(Dependency condition, ScopeBlock then_statements, ScopeBlock else_statements = {}) noexcept;
		~IfNode() override = default;

		/**
		 * @brief Construct new If node.
		 * @param condition Condition to be evaluated.
		 * @param then_statements Statements executed if the condition evaluates to true.
		 * @param else_statements [Optional] Statements executed if the condition evaluates to false.
		 * @return New 'IfNode' node.
		 */
		static Dependency create(Dependency condition, ScopeBlock then_statements, ScopeBlock else_statements = {});
	};

	/**
	 * @brief Represents a 'Literal' expression in the Abstract Syntax Tree (AST).
	 */
	class LiteralNode : public VisitorAcceptor<LiteralNode>
	{
		public:
		Value value{};

		public:
		LiteralNode(Value value);
		~LiteralNode() override = default;
		operator std::string() const noexcept;

		/**
		 * @brief Constructs new Literal node.
		 * @param type Type of stored Value, i.e. identifier, string, int32, etc.
		 * @param value Value associated with the literal.
		 * @return New 'LiteralNode' node.
		 */
		static Dependency create(Value value);
	};

	/**
	 * @brief Represents a `Continue/Break` statement (in loops) in the Abstract Syntax Tree (AST).
	 */
	class LoopControlNode final : public VisitorAcceptor<LoopControlNode>
	{
		public:
		enum class Type : bool
		{
			Break,
			Continue,
		};

		public:
		Type control_type;

		public:
		explicit LoopControlNode(Type control_type) noexcept;
		~LoopControlNode() override = default;

		/**
		 * @brief Construct new LoopControl node.
		 * @param control_type Type of the control statement, either `break` or `continue.
		 * @return New 'LoopControlNode' node.
		 */
		static Dependency create(Type control_type);
	};

	/**
	 * @brief Represents a 'root' node containing all the statements of a given script
	 * associated under common name in the Abstract Syntax Tree (AST)
	 */
	class ModuleNode : public VisitorAcceptor<ModuleNode>
	{
		public:
		Identifier name;
		Dependencies statements;

		public:
		explicit ModuleNode(Identifier module_name, Dependencies statements) noexcept;
		~ModuleNode() override = default;

		/**
		 * @brief Construct new Module node.
		 * @param module_name Name of the module.
		 * @param statements All the statements making up the module.
		 * @return new 'Module' node.
		 */
		static Dependency create(Identifier module_name, Dependencies statements);
	};

	/**
	 * @brief Represents a `Return` statement in the Abstract Syntax Tree (AST).
	 */
	class ReturnNode final : public VisitorAcceptor<ReturnNode>
	{
		public:
		Dependency expression;

		public:
		explicit ReturnNode(Dependency expression);
		~ReturnNode() override = default;

		/**
		 * @brief Construct new Return node.
		 * @param expression [Optional] Expression to be returned.
		 * @return New 'ReturnNode node.
		 */
		static Dependency create(Dependency expression = {});
	};

	/**
	 * @brief Represents a 'Struct Declaration' statement
	 * in the Abstract Syntax Tree (AST).
	 */
	class StructDeclarationNode final : public VisitorAcceptor<StructDeclarationNode>
	{
		public:
		Identifier name;
		Dependencies parameters;

		public:
		explicit StructDeclarationNode(Identifier name, Dependencies parameters);
		~StructDeclarationNode() override = default;

		/**
		 * @brief Constructs new Struct Declaration statement node.
		 * @param name Name of the Struct to be identified by.
		 * @param statements Variable declarations making this struct.
		 * @return New 'Struct Declaration' statement node.
		 */
		static Dependency create(Identifier name, Dependencies statements);
	};

	/**
	 * @brief Represents an 'Unary' expression in the Abstract Syntax Tree (AST).
	 * Contains an operand bound by an operator.
	 */
	class UnaryNode final : public VisitorAcceptor<UnaryNode>
	{
		public:
		Operator op;
		Dependency expression;

		public:
		explicit UnaryNode(Dependency expr, Operator op);
		~UnaryNode() override = default;

		/**
		 * @brief Constructs new Unary expression node.
		 * @param expr Expression to be bound by an operator.
		 * @param op Operator binding the expression.
		 * @return New 'Unary' expression node.
		 */
		static Dependency create(Dependency expr, Operator op);
	};

	/**
	 * @brief Represents a 'Variable Declaration' statement
	 * in the Abstract Syntax Tree (AST).
	 */
	class VariableDeclarationNode final : public VisitorAcceptor<VariableDeclarationNode>
	{
		public:
		Identifier name{};
		parser::TypeSpecifier type_specifier;
		Dependency expression{};
		bool is_mutable{};

		public:
		explicit VariableDeclarationNode(Identifier name, parser::TypeSpecifier type_specifier, Dependency expr, bool is_mutable);
		~VariableDeclarationNode() override = default;

		/**
		 * @brief Constructs new Variable Declaration statement node.
		 * @param name Name of the Variable to be identified by.
		 * @param type_specifier Type of the variable.
		 * @param expr Expression this variable evaluates to.
		 * @param is_mutable Can the value of the expression be reassigned, i.e. is const?
		 * @return New 'Variable Declaration' statement node.
		 */
		static Dependency create(Identifier name, parser::TypeSpecifier type_specifier, Dependency expr, bool is_mutable);
	};

	/**
	 * @brief Represents a `While` statement in the Abstract Syntax Tree (AST).
	 */
	class WhileNode final : public VisitorAcceptor<WhileNode>
	{
		public:
		Dependency condition;
		ScopeBlock statements;

		public:
		explicit WhileNode(Dependency condition, ScopeBlock statements) noexcept;
		~WhileNode() override = default;

		/**
		 * @brief Constructs new WhileLoop expression statement node.
		 * @param condition Expression verified after each loop iteration.
		 * @param statements List of statements to execute each loop.
		 * @return New 'WhileLoop' expression statement node.
		 */
		static Dependency create(Dependency condition, ScopeBlock statements);
	};
}  // namespace soul::ast
