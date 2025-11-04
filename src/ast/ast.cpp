#include "ast/ast.h"

#include <unordered_map>

namespace soul::ast
{
	std::string_view ASTNode::name(const ASTNode::Operator op) noexcept
	{
		using namespace std::string_view_literals;
		static const std::unordered_map<ASTNode::Operator, std::string_view> k_operators = {
			{ Operator::Unknown,      "__unknown__"sv },
			{ Operator::Add,          "+"sv           },
			{ Operator::Sub,          "-"sv           },
			{ Operator::Mul,          "*"sv           },
			{ Operator::Div,          "/"sv           },
			{ Operator::Mod,          "%"sv           },
			{ Operator::Increment,    "++"sv          },
			{ Operator::Decrement,    "--"sv          },
			{ Operator::Equal,        "="sv           },
			{ Operator::NotEqual,     "!="sv          },
			{ Operator::Greater,      ">"sv           },
			{ Operator::GreaterEqual, ">="sv          },
			{ Operator::Less,         "<"sv           },
			{ Operator::LessEqual,    "<="sv          },
			{ Operator::LogicalNot,   "!"sv           },
			{ Operator::LogicalAnd,   "&&"sv          },
			{ Operator::LogicalOr,    "||"sv          },
		};
		if (!k_operators.contains(op)) [[unlikely]] {
			return k_operators.at(Operator::Unknown);
		}
		return k_operators.at(op);
	}

	std::string_view ASTNode::internal_name(const Operator op) noexcept
	{
		using namespace std::string_view_literals;
		static const std::unordered_map<ASTNode::Operator, std::string_view> k_operators = {
			{ Operator::Unknown,      "__unknown__"sv            },
			{ Operator::Assign,       "operator_assign"sv        },
			{ Operator::AddAssign,    "operator_add_assign"sv    },
			{ Operator::SubAssign,    "operator_sub_assign"sv    },
			{ Operator::MulAssign,    "operator_mul_assign"sv    },
			{ Operator::DivAssign,    "operator_div_assign"sv    },
			{ Operator::ModAssign,    "operator_mod_assign"sv    },
			{ Operator::Add,          "operator_add"sv           },
			{ Operator::Sub,          "operator_sub"sv           },
			{ Operator::Mul,          "operator_mul"sv           },
			{ Operator::Div,          "operator_div"sv           },
			{ Operator::Mod,          "operator_mod"sv           },
			{ Operator::Increment,    "operator_increment"sv     },
			{ Operator::Decrement,    "operator_decrement"sv     },
			{ Operator::Equal,        "operator_equal"sv         },
			{ Operator::NotEqual,     "operator_not_equal"sv     },
			{ Operator::Greater,      "operator_greater"sv       },
			{ Operator::GreaterEqual, "operator_greater_equal"sv },
			{ Operator::Less,         "operator_less"sv          },
			{ Operator::LessEqual,    "operator_less_equal"sv    },
			{ Operator::LogicalNot,   "operator_logical_not"sv   },
			{ Operator::LogicalAnd,   "operator_logical_and"sv   },
			{ Operator::LogicalOr,    "operator_logical_or"sv    },
		};
		if (!k_operators.contains(op)) [[unlikely]] {
			return k_operators.at(Operator::Unknown);
		}
		return k_operators.at(op);
	}

	ASTNode::Operator ASTNode::as_operator(Token::Type type) noexcept
	{
		static const std::unordered_map<Token::Type, Operator> k_operators = {
			{ Token::Type::SymbolEqual,              Operator::Assign       },
			{ Token::Type::SymbolPlusEqual,          Operator::AddAssign    },
			{ Token::Type::SymbolMinusEqual,         Operator::SubAssign    },
			{ Token::Type::SymbolStarEqual,          Operator::MulAssign    },
			{ Token::Type::SymbolSlashEqual,         Operator::DivAssign    },
			{ Token::Type::SymbolPercentEqual,       Operator::ModAssign    },
			{ Token::Type::SymbolPlus,               Operator::Add          },
			{ Token::Type::SymbolMinus,              Operator::Sub          },
			{ Token::Type::SymbolStar,               Operator::Mul          },
			{ Token::Type::SymbolSlash,              Operator::Div          },
			{ Token::Type::SymbolPercent,            Operator::Mod          },
			{ Token::Type::SymbolPlusPlus,           Operator::Increment    },
			{ Token::Type::SymbolMinusMinus,         Operator::Decrement    },
			{ Token::Type::SymbolEqual,              Operator::Equal        },
			{ Token::Type::SymbolBangEqual,          Operator::NotEqual     },
			{ Token::Type::SymbolGreater,            Operator::Greater      },
			{ Token::Type::SymbolGreaterEqual,       Operator::GreaterEqual },
			{ Token::Type::SymbolLess,               Operator::Less         },
			{ Token::Type::SymbolLessEqual,          Operator::LessEqual    },
			{ Token::Type::SymbolBang,               Operator::LogicalNot   },
			{ Token::Type::SymbolAmpersandAmpersand, Operator::LogicalAnd   },
			{ Token::Type::SymbolPipePipe,           Operator::LogicalOr    },
		};
		if (!k_operators.contains(type)) {
			return Operator::Unknown;
		}
		return k_operators.at(type);
	}

	BinaryNode::BinaryNode(Dependency lhs, Dependency rhs, Operator op)
		: op(op), lhs(std::move(lhs)), rhs(std::move(rhs))
	{
	}

	BinaryNode::Dependency BinaryNode::create(Dependency lhs, Dependency rhs, Operator op)
	{
		return std::make_unique<BinaryNode>(std::move(lhs), std::move(rhs), op);
	}

	BlockNode::BlockNode(Dependencies dependencies) : statements(std::move(dependencies)) {}

	BlockNode::Dependency BlockNode::create(BlockNode::Dependencies statements)
	{
		return std::make_unique<BlockNode>(std::move(statements));
	}

	CastNode::CastNode(Dependency expression, Identifier type_identifier)
		: expression(std::move(expression)), type_identifier(std::move(type_identifier))
	{
	}

	CastNode::Dependency CastNode::create(Dependency expression, Identifier type_identifier)
	{
		return std::make_unique<CastNode>(std::move(expression), std::move(type_identifier));
	}

	ErrorNode::ErrorNode(ErrorNode::Message message) : message(std::move(message)) {}

	ASTNode::Dependency ErrorNode::create(ErrorNode::Message message)
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

	ForLoopNode::Dependency ForLoopNode::create(Dependency initialization,
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

	ForeachLoopNode::Dependency ForeachLoopNode::create(Dependency variable,
	                                                    Dependency in_expression,
	                                                    ScopeBlock statements)
	{
		return std::make_unique<ForeachLoopNode>(std::move(variable), std::move(in_expression), std::move(statements));
	}

	FunctionCallNode::FunctionCallNode(Identifier name, Dependencies parameters)
		: name(std::move(name)), parameters(std::move(parameters))
	{
	}

	FunctionCallNode::Dependency FunctionCallNode::create(Identifier name, Dependencies parameters)
	{
		return std::make_unique<FunctionCallNode>(std::move(name), std::move(parameters));
	}

	FunctionDeclarationNode::FunctionDeclarationNode(Identifier identifier,
	                                                 Identifier return_type_identifier,
	                                                 Dependencies parameters,
	                                                 ScopeBlock statements)
		: name(std::move(identifier)),
		  type_identifier(std::move(return_type_identifier)),
		  parameters(std::move(parameters)),
		  statements(std::move(statements))
	{
	}

	FunctionDeclarationNode::Dependency FunctionDeclarationNode::create(Identifier name,
	                                                                    Identifier return_type,
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

	IfNode::Dependency IfNode::create(Dependency condition, ScopeBlock then_statements, ScopeBlock else_statements)
	{
		return std::make_unique<IfNode>(std::move(condition), std::move(then_statements), std::move(else_statements));
	}

	LiteralNode::LiteralNode(Value value) : value(std::move(value)) {}

	LiteralNode::Dependency LiteralNode::create(Value value) { return std::make_unique<LiteralNode>(std::move(value)); }

	LiteralNode::operator std::string() const noexcept { return std::string(value); }

	LoopControlNode::LoopControlNode(Type control_type) noexcept : control_type(control_type) {}

	LoopControlNode::Dependency LoopControlNode::create(Type control_type)
	{
		return std::make_unique<LoopControlNode>(control_type);
	}

	ModuleNode::ModuleNode(Identifier module_name, Dependencies statements) noexcept
		: name(std::move(module_name)), statements(std::move(statements))
	{
	}

	ASTNode::Dependency ModuleNode::create(Identifier module_name, Dependencies statements)
	{
		return std::make_unique<ModuleNode>(std::move(module_name), std::move(statements));
	}

	ReturnNode::ReturnNode(Dependency expression) : expression(std::move(expression)) {}

	ReturnNode::Dependency ReturnNode::create(Dependency expression)
	{
		return std::make_unique<ReturnNode>(std::move(expression));
	}

	StructDeclarationNode::StructDeclarationNode(Identifier name, Dependencies parameters)
		: name(std::move(name)), parameters(std::move(parameters))
	{
	}

	StructDeclarationNode::Dependency StructDeclarationNode::create(Identifier name, Dependencies parameters)
	{
		return std::make_unique<StructDeclarationNode>(std::move(name), std::move(parameters));
	}

	UnaryNode::UnaryNode(Dependency expr, Operator op) : op(op), expression(std::move(expr)) {}

	UnaryNode::Dependency UnaryNode::create(Dependency expr, Operator op)
	{
		return std::make_unique<UnaryNode>(std::move(expr), op);
	}

	VariableDeclarationNode::VariableDeclarationNode(Identifier name, Identifier type, Dependency expr, bool is_mutable)
		: name(std::move(name)), type_identifier(std::move(type)), expression(std::move(expr)), is_mutable(is_mutable)
	{
	}

	VariableDeclarationNode::Dependency VariableDeclarationNode::create(Identifier name,
	                                                                    Identifier type,
	                                                                    Dependency expr,
	                                                                    bool is_mutable)
	{
		return std::make_unique<VariableDeclarationNode>(std::move(name), std::move(type), std::move(expr), is_mutable);
	}

	WhileNode::WhileNode(ASTNode::Dependency condition, ASTNode::ScopeBlock statements) noexcept
		: condition(std::move(condition)), statements(std::move(statements))
	{
	}

	ASTNode::Dependency WhileNode::create(ASTNode::Dependency condition, ASTNode::ScopeBlock statements)
	{
		return std::make_unique<WhileNode>(std::move(condition), std::move(statements));
	}
}  // namespace soul::ast
