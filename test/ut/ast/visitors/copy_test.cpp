#include "ast/visitors/copy.h"

#include <gtest/gtest.h>

#include "ast/ast.h"
#include "ast/visitors/compare.h"
#include "ast/visitors/stringify.h"

#include <string>

namespace soul::ast::visitors
{
	using namespace soul::ast;
	using namespace soul::types;

	class CopyVisitorTest : public ::testing::Test
	{
	};

	TEST_F(CopyVisitorTest, All)
	{
		auto struct_declaration_members = ASTNode::Dependencies{};
		struct_declaration_members.reserve(3);
		struct_declaration_members.emplace_back(VariableDeclarationNode::create("my_int", "i32", nullptr, false));
		struct_declaration_members.emplace_back(VariableDeclarationNode::create("my_str", "str", nullptr, false));
		struct_declaration_members.emplace_back(VariableDeclarationNode::create("my_float", "f64", nullptr, false));
		auto struct_declaration = StructDeclarationNode::create("my_struct", std::move(struct_declaration_members));

		auto if_node_true_statements = ASTNode::Dependencies{};
		if_node_true_statements.emplace_back(
			UnaryNode::create(LiteralNode::create(Identifier::create("index"), LiteralNode::Type::Identifier),
		                      ASTNode::Operator::Decrement));
		auto if_node_false_statements = ASTNode::Dependencies{};
		if_node_false_statements.emplace_back(
			UnaryNode::create(LiteralNode::create(Identifier::create("index"), LiteralNode::Type::Identifier),
		                      ASTNode::Operator::Increment));

		auto if_node = IfNode::create(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Boolean>(true), LiteralNode::Type::Boolean),
			BlockNode::create(std::move(if_node_true_statements)),
			BlockNode::create(std::move(if_node_false_statements)));

		auto for_loop_initialization = VariableDeclarationNode::create(
			"index",
			"i32",
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(0), LiteralNode::Type::Int32),
			false);
		auto for_loop_condition = BinaryNode::create(
			LiteralNode::create(Identifier::create("index"), LiteralNode::Type::Identifier),
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(10), LiteralNode::Type::Int32),
			ASTNode::Operator::LessEqual);
		auto for_loop_update
			= UnaryNode::create(LiteralNode::create(Identifier::create("index"), LiteralNode::Type::Identifier),
		                        ASTNode::Operator::Increment);
		auto for_loop_statements = ASTNode::Dependencies{};
		for_loop_statements.push_back(std::move(if_node));

		auto for_loop = ForLoopNode::create(std::move(for_loop_initialization),
		                                    std::move(for_loop_condition),
		                                    std::move(for_loop_update),
		                                    BlockNode::create(std::move(for_loop_statements)));

		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.reserve(3);
		function_declaration_statements.push_back(std::move(for_loop));
		function_declaration_statements.emplace_back(LoopControlNode::create(LoopControlNode::Type::Continue));
		function_declaration_statements.emplace_back(ReturnNode::create(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(123), LiteralNode::Type::Int32)));
		auto function_declaration_parameters = ASTNode::Dependencies{};
		function_declaration_parameters.reserve(2);
		function_declaration_parameters.emplace_back(VariableDeclarationNode::create("a", "i32", nullptr, false));
		function_declaration_parameters.emplace_back(VariableDeclarationNode::create("b", "chr", nullptr, false));
		auto function_declaration
			= FunctionDeclarationNode::create("my_function",
		                                      "my_struct",
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(2);
		module_statements.push_back(std::move(struct_declaration));
		module_statements.push_back(std::move(function_declaration));
		auto expected_module = ModuleNode::create("copy_module", std::move(module_statements));

		CopyVisitor copy_visitor{};
		copy_visitor.accept(expected_module.get());

		const auto& result_module = copy_visitor.cloned();
		ASSERT_TRUE(CompareVisitor(expected_module.get(), result_module.get()));
	}
}  // namespace soul::ast::visitors
