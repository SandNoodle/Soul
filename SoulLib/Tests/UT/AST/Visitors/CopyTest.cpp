#include "AST/Visitors/Copy.h"

#include <gtest/gtest.h>

#include "AST/AST.h"
#include "AST/Visitors/Compare.h"

#include <string>

namespace Soul::AST::Visitors::UT
{
	using namespace Soul::AST;
	using namespace Soul::Parser;
	using namespace Soul::Types;

	class CopyVisitorTest : public ::testing::Test
	{
	};

	TEST_F(CopyVisitorTest, All)
	{
		auto struct_declaration_members = ASTNode::Dependencies{};
		struct_declaration_members.reserve(3);
		struct_declaration_members.emplace_back(
			VariableDeclarationNode::Create("my_int", k_base_specifier_i32, nullptr, false));
		struct_declaration_members.emplace_back(
			VariableDeclarationNode::Create("my_str", k_base_specifier_str, nullptr, false));
		struct_declaration_members.emplace_back(
			VariableDeclarationNode::Create("my_float", k_base_specifier_f64, nullptr, false));
		auto struct_declaration = StructDeclarationNode::Create("my_struct", std::move(struct_declaration_members));

		auto if_node_true_statements = ASTNode::Dependencies{};
		if_node_true_statements.emplace_back(
			UnaryNode::Create(LiteralNode::Create(Identifier::create("index")), ASTNode::Operator::DECREMENT));
		auto if_node_false_statements = ASTNode::Dependencies{};
		if_node_false_statements.emplace_back(
			UnaryNode::Create(LiteralNode::Create(Identifier::create("index")), ASTNode::Operator::INCREMENT));

		auto if_node = IfNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true)),
		                              BlockNode::Create(std::move(if_node_true_statements)),
		                              BlockNode::Create(std::move(if_node_false_statements)));

		auto for_loop_initialization = VariableDeclarationNode::Create(
			"index", k_base_specifier_i32, LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(0)), false);
		auto for_loop_condition
			= BinaryNode::Create(LiteralNode::Create(Identifier::create("index")),
		                         LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(10)),
		                         ASTNode::Operator::LESS_EQUAL);
		auto for_loop_update
			= UnaryNode::Create(LiteralNode::Create(Identifier::create("index")), ASTNode::Operator::INCREMENT);
		auto for_loop_statements = ASTNode::Dependencies{};
		for_loop_statements.push_back(std::move(if_node));

		auto for_loop = ForLoopNode::Create(std::move(for_loop_initialization),
		                                    std::move(for_loop_condition),
		                                    std::move(for_loop_update),
		                                    BlockNode::Create(std::move(for_loop_statements)));

		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.reserve(3);
		function_declaration_statements.push_back(std::move(for_loop));
		function_declaration_statements.emplace_back(LoopControlNode::Create(LoopControlNode::Type::CONTINUE));
		function_declaration_statements.emplace_back(
			ReturnNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(123))));
		auto function_declaration_parameters = ASTNode::Dependencies{};
		function_declaration_parameters.reserve(2);
		function_declaration_parameters.emplace_back(
			VariableDeclarationNode::Create("a", k_base_specifier_i32, nullptr, false));
		function_declaration_parameters.emplace_back(
			VariableDeclarationNode::Create("b", k_base_specifier_chr, nullptr, false));
		auto function_declaration
			= FunctionDeclarationNode::Create("my_function",
		                                      BaseTypeSpecifier{ "my_struct" },
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::Create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(2);
		module_statements.push_back(std::move(struct_declaration));
		module_statements.push_back(std::move(function_declaration));
		auto expected_module = ModuleNode::Create("copy_module", std::move(module_statements));

		CopyVisitor copy_visitor{};
		copy_visitor.Accept(expected_module.get());

		const auto& result_module = copy_visitor.Cloned();
		ASSERT_TRUE(CompareVisitor(expected_module.get(), result_module.get()));
	}
}  // namespace Soul::AST::Visitors::UT
