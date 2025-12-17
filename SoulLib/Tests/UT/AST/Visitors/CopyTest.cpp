#include "AST/Visitors/copy.h"

#include <gtest/gtest.h>

#include "AST/Visitors/compare.h"
#include "AST/ast.h"

#include <string>

namespace soul::ast::visitors
{
	using namespace soul::ast;
	using namespace soul::parser;
	using namespace soul::types;

	class CopyVisitorTest : public ::testing::Test
	{
	};

	TEST_F(CopyVisitorTest, All)
	{
		auto struct_declaration_members = ASTNode::Dependencies{};
		struct_declaration_members.reserve(3);
		struct_declaration_members.emplace_back(
			VariableDeclarationNode::create("my_int", k_base_specifier_i32, nullptr, false));
		struct_declaration_members.emplace_back(
			VariableDeclarationNode::create("my_str", k_base_specifier_str, nullptr, false));
		struct_declaration_members.emplace_back(
			VariableDeclarationNode::create("my_float", k_base_specifier_f64, nullptr, false));
		auto struct_declaration = StructDeclarationNode::create("my_struct", std::move(struct_declaration_members));

		auto if_node_true_statements = ASTNode::Dependencies{};
		if_node_true_statements.emplace_back(
			UnaryNode::create(LiteralNode::create(Identifier::create("index")), ASTNode::Operator::Decrement));
		auto if_node_false_statements = ASTNode::Dependencies{};
		if_node_false_statements.emplace_back(
			UnaryNode::create(LiteralNode::create(Identifier::create("index")), ASTNode::Operator::Increment));

		auto if_node = IfNode::create(LiteralNode::create(Scalar::create<PrimitiveType::Kind::Boolean>(true)),
		                              BlockNode::create(std::move(if_node_true_statements)),
		                              BlockNode::create(std::move(if_node_false_statements)));

		auto for_loop_initialization = VariableDeclarationNode::create(
			"index", k_base_specifier_i32, LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(0)), false);
		auto for_loop_condition
			= BinaryNode::create(LiteralNode::create(Identifier::create("index")),
		                         LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(10)),
		                         ASTNode::Operator::LessEqual);
		auto for_loop_update
			= UnaryNode::create(LiteralNode::create(Identifier::create("index")), ASTNode::Operator::Increment);
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
		function_declaration_statements.emplace_back(
			ReturnNode::create(LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(123))));
		auto function_declaration_parameters = ASTNode::Dependencies{};
		function_declaration_parameters.reserve(2);
		function_declaration_parameters.emplace_back(
			VariableDeclarationNode::create("a", k_base_specifier_i32, nullptr, false));
		function_declaration_parameters.emplace_back(
			VariableDeclarationNode::create("b", k_base_specifier_chr, nullptr, false));
		auto function_declaration
			= FunctionDeclarationNode::create("my_function",
		                                      BaseTypeSpecifier{ "my_struct" },
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
