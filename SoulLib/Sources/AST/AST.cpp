#include "AST/AST.h"

#include <unordered_map>

namespace Soul::AST
{
	using namespace Soul::Lexer;
	using namespace Soul::Parser;

	std::string_view ASTNode::Name(const ASTNode::Operator op) noexcept
	{
		using namespace std::string_view_literals;
		static const std::unordered_map<ASTNode::Operator, std::string_view> k_operators = {
			{ Operator::UNKNOWN,       "__unknown__"sv },
			{ Operator::ASSIGN,        "="sv           },
			{ Operator::ASSIGN_ADD,    "+="sv          },
			{ Operator::ASSIGN_SUB,    "-="sv          },
			{ Operator::ASSIGN_MUL,    "*="sv          },
			{ Operator::ASSIGN_DIV,    "/="sv          },
			{ Operator::ASSIGN_MOD,    "%="sv          },
			{ Operator::ADD,           "+"sv           },
			{ Operator::SUB,           "-"sv           },
			{ Operator::MUL,           "*"sv           },
			{ Operator::DIV,           "/"sv           },
			{ Operator::MOD,           "%"sv           },
			{ Operator::INCREMENT,     "++"sv          },
			{ Operator::DECREMENT,     "--"sv          },
			{ Operator::EQUAL,         "="sv           },
			{ Operator::NOT_EQUAL,     "!="sv          },
			{ Operator::GREATER,       ">"sv           },
			{ Operator::GREATER_EQUAL, ">="sv          },
			{ Operator::LESS,          "<"sv           },
			{ Operator::LESS_EQUAL,    "<="sv          },
			{ Operator::LOGICAL_NOT,   "!"sv           },
			{ Operator::LOGICAL_AND,   "&&"sv          },
			{ Operator::LOGICAL_OR,    "||"sv          },
		};
		if (!k_operators.contains(op)) [[unlikely]] {
			return k_operators.at(Operator::UNKNOWN);
		}
		return k_operators.at(op);
	}

	std::string_view ASTNode::NameInternal(const Operator op) noexcept
	{
		using namespace std::string_view_literals;
		static const std::unordered_map<ASTNode::Operator, std::string_view> k_operators = {
			{ Operator::UNKNOWN,       "__unknown__"sv            },
			{ Operator::ASSIGN,        "operator_assign"sv        },
			{ Operator::ASSIGN_ADD,    "operator_add_assign"sv    },
			{ Operator::ASSIGN_SUB,    "operator_sub_assign"sv    },
			{ Operator::ASSIGN_MUL,    "operator_mul_assign"sv    },
			{ Operator::ASSIGN_DIV,    "operator_div_assign"sv    },
			{ Operator::ASSIGN_MOD,    "operator_mod_assign"sv    },
			{ Operator::ADD,           "operator_add"sv           },
			{ Operator::SUB,           "operator_sub"sv           },
			{ Operator::MUL,           "operator_mul"sv           },
			{ Operator::DIV,           "operator_div"sv           },
			{ Operator::MOD,           "operator_mod"sv           },
			{ Operator::INCREMENT,     "operator_increment"sv     },
			{ Operator::DECREMENT,     "operator_decrement"sv     },
			{ Operator::EQUAL,         "operator_equal"sv         },
			{ Operator::NOT_EQUAL,     "operator_not_equal"sv     },
			{ Operator::GREATER,       "operator_greater"sv       },
			{ Operator::GREATER_EQUAL, "operator_greater_equal"sv },
			{ Operator::LESS,          "operator_less"sv          },
			{ Operator::LESS_EQUAL,    "operator_less_equal"sv    },
			{ Operator::LOGICAL_NOT,   "operator_logical_not"sv   },
			{ Operator::LOGICAL_AND,   "operator_logical_and"sv   },
			{ Operator::LOGICAL_OR,    "operator_logical_or"sv    },
		};
		if (!k_operators.contains(op)) [[unlikely]] {
			return k_operators.at(Operator::UNKNOWN);
		}
		return k_operators.at(op);
	}

	ASTNode::Operator ASTNode::AsOperator(Token::Type type) noexcept
	{
		static const std::unordered_map<Token::Type, Operator> k_operators = {
			{ Token::Type::SYMBOL_EQUAL,               Operator::ASSIGN        },
			{ Token::Type::SYMBOL_PLUS_EQUAL,          Operator::ASSIGN_ADD    },
			{ Token::Type::SYMBOL_MINUS_EQUAL,         Operator::ASSIGN_SUB    },
			{ Token::Type::SYMBOL_STAR_EQUAL,          Operator::ASSIGN_MUL    },
			{ Token::Type::SYMBOL_SLASH_EQUAL,         Operator::ASSIGN_DIV    },
			{ Token::Type::SYMBOL_PERCENT_EQUAL,       Operator::ASSIGN_MOD    },
			{ Token::Type::SYMBOL_PLUS,                Operator::ADD           },
			{ Token::Type::SYMBOL_MINUS,               Operator::SUB           },
			{ Token::Type::SYMBOL_STAR,                Operator::MUL           },
			{ Token::Type::SYMBOL_SLASH,               Operator::DIV           },
			{ Token::Type::SYMBOL_PERCENT,             Operator::MOD           },
			{ Token::Type::SYMBOL_PLUS_PLUS,           Operator::INCREMENT     },
			{ Token::Type::SYMBOL_MINUS_MINUS,         Operator::DECREMENT     },
			{ Token::Type::SYMBOL_EQUAL,               Operator::EQUAL         },
			{ Token::Type::SYMBOL_BANG_EQUAL,          Operator::NOT_EQUAL     },
			{ Token::Type::SYMBOL_GREATER,             Operator::GREATER       },
			{ Token::Type::SYMBOL_GREATER_EQUAL,       Operator::GREATER_EQUAL },
			{ Token::Type::SYMBOL_LESS,                Operator::LESS          },
			{ Token::Type::SYMBOL_LESS_EQUAL,          Operator::LESS_EQUAL    },
			{ Token::Type::SYMBOL_BANG,                Operator::LOGICAL_NOT   },
			{ Token::Type::SYMBOL_AMPERSAND_AMPERSAND, Operator::LOGICAL_AND   },
			{ Token::Type::SYMBOL_PIPE_PIPE,           Operator::LOGICAL_OR    },
		};
		if (!k_operators.contains(type)) {
			return Operator::UNKNOWN;
		}
		return k_operators.at(type);
	}

	BinaryNode::BinaryNode(Dependency lhs, Dependency rhs, Operator op)
		: op(op), lhs(std::move(lhs)), rhs(std::move(rhs))
	{
	}

	BinaryNode::Dependency BinaryNode::Create(Dependency lhs, Dependency rhs, Operator op)
	{
		return std::make_unique<BinaryNode>(std::move(lhs), std::move(rhs), op);
	}

	BlockNode::BlockNode(Dependencies dependencies) : statements(std::move(dependencies)) {}

	BlockNode::Dependency BlockNode::Create(BlockNode::Dependencies statements)
	{
		return std::make_unique<BlockNode>(std::move(statements));
	}

	CastNode::CastNode(Dependency expression, TypeSpecifier type_specifier)
		: expression(std::move(expression)), type_specifier(std::move(type_specifier))
	{
	}

	CastNode::Dependency CastNode::Create(Dependency expression, TypeSpecifier type_specifier)
	{
		return std::make_unique<CastNode>(std::move(expression), std::move(type_specifier));
	}

	ErrorNode::ErrorNode(ErrorNode::Message message) : message(std::move(message)) {}

	ASTNode::Dependency ErrorNode::Create(ErrorNode::Message message)
	{
		return std::make_unique<ErrorNode>(std::move(message));
	}

	ForLoopNode::ForLoopNode(Dependency initialization,
	                         Dependency condition,
	                         Dependency update,
	                         ScopeBlock statements) noexcept
		: initialization(std::move(initialization)),
		  condition(std::move(condition)),
		  update(std::move(update)),
		  statements(std::move(statements))
	{
	}

	ForLoopNode::Dependency ForLoopNode::Create(Dependency initialization,
	                                            Dependency condition,
	                                            Dependency update,
	                                            ScopeBlock statements)
	{
		return std::make_unique<ForLoopNode>(
			std::move(initialization), std::move(condition), std::move(update), std::move(statements));
	}

	ForeachLoopNode::ForeachLoopNode(Dependency variable, Dependency in_expression, ScopeBlock statements) noexcept
		: variable(std::move(variable)), in_expression(std::move(in_expression)), statements(std::move(statements))
	{
	}

	ForeachLoopNode::Dependency ForeachLoopNode::Create(Dependency variable,
	                                                    Dependency in_expression,
	                                                    ScopeBlock statements)
	{
		return std::make_unique<ForeachLoopNode>(std::move(variable), std::move(in_expression), std::move(statements));
	}

	FunctionCallNode::FunctionCallNode(Identifier name, Dependencies parameters)
		: name(std::move(name)), parameters(std::move(parameters))
	{
	}

	FunctionCallNode::Dependency FunctionCallNode::Create(Identifier name, Dependencies parameters)
	{
		return std::make_unique<FunctionCallNode>(std::move(name), std::move(parameters));
	}

	FunctionDeclarationNode::FunctionDeclarationNode(Identifier identifier,
	                                                 TypeSpecifier return_type_specifier,
	                                                 Dependencies parameters,
	                                                 ScopeBlock statements)
		: name(std::move(identifier)),
		  type_specifier(std::move(return_type_specifier)),
		  parameters(std::move(parameters)),
		  statements(std::move(statements))
	{
	}

	FunctionDeclarationNode::Dependency FunctionDeclarationNode::Create(Identifier name,
	                                                                    TypeSpecifier return_type,
	                                                                    Dependencies parameters,
	                                                                    ScopeBlock statements)
	{
		return std::make_unique<FunctionDeclarationNode>(
			std::move(name), std::move(return_type), std::move(parameters), std::move(statements));
	}

	IfNode::IfNode(Dependency condition, ScopeBlock then_statements, ScopeBlock else_statements) noexcept
		: condition(std::move(condition)),
		  then_statements(std::move(then_statements)),
		  else_statements(std::move(else_statements))
	{
	}

	IfNode::Dependency IfNode::Create(Dependency condition, ScopeBlock then_statements, ScopeBlock else_statements)
	{
		return std::make_unique<IfNode>(std::move(condition), std::move(then_statements), std::move(else_statements));
	}

	LiteralNode::LiteralNode(Value value) : value(std::move(value)) {}

	LiteralNode::Dependency LiteralNode::Create(Value value) { return std::make_unique<LiteralNode>(std::move(value)); }

	LiteralNode::operator std::string() const noexcept { return std::string(value); }

	LoopControlNode::LoopControlNode(Type control_type) noexcept : control_type(control_type) {}

	LoopControlNode::Dependency LoopControlNode::Create(Type control_type)
	{
		return std::make_unique<LoopControlNode>(control_type);
	}

	ModuleNode::ModuleNode(Identifier module_name, Dependencies statements) noexcept
		: name(std::move(module_name)), statements(std::move(statements))
	{
	}

	ASTNode::Dependency ModuleNode::Create(Identifier module_name, Dependencies statements)
	{
		return std::make_unique<ModuleNode>(std::move(module_name), std::move(statements));
	}

	ReturnNode::ReturnNode(Dependency expression) : expression(std::move(expression)) {}

	ReturnNode::Dependency ReturnNode::Create(Dependency expression)
	{
		return std::make_unique<ReturnNode>(std::move(expression));
	}

	StructDeclarationNode::StructDeclarationNode(Identifier name, Dependencies parameters)
		: name(std::move(name)), parameters(std::move(parameters))
	{
	}

	StructDeclarationNode::Dependency StructDeclarationNode::Create(Identifier name, Dependencies parameters)
	{
		return std::make_unique<StructDeclarationNode>(std::move(name), std::move(parameters));
	}

	UnaryNode::UnaryNode(Dependency expr, Operator op) : op(op), expression(std::move(expr)) {}

	UnaryNode::Dependency UnaryNode::Create(Dependency expr, Operator op)
	{
		return std::make_unique<UnaryNode>(std::move(expr), op);
	}

	VariableDeclarationNode::VariableDeclarationNode(Identifier name,
	                                                 TypeSpecifier type_specifier,
	                                                 Dependency expr,
	                                                 bool is_mutable)
		: name(std::move(name)),
		  type_specifier(std::move(type_specifier)),
		  expression(std::move(expr)),
		  is_mutable(is_mutable)
	{
	}

	VariableDeclarationNode::Dependency VariableDeclarationNode::Create(Identifier name,
	                                                                    TypeSpecifier type_specifier,
	                                                                    Dependency expr,
	                                                                    bool is_mutable)
	{
		return std::make_unique<VariableDeclarationNode>(
			std::move(name), std::move(type_specifier), std::move(expr), is_mutable);
	}

	WhileNode::WhileNode(ASTNode::Dependency condition, ASTNode::ScopeBlock statements) noexcept
		: condition(std::move(condition)), statements(std::move(statements))
	{
	}

	ASTNode::Dependency WhileNode::Create(ASTNode::Dependency condition, ASTNode::ScopeBlock statements)
	{
		return std::make_unique<WhileNode>(std::move(condition), std::move(statements));
	}
}  // namespace Soul::AST
