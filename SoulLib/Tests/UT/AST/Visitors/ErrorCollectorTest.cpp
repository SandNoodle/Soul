#include "AST/Visitors/ErrorCollector.h"

#include <gtest/gtest.h>

#include "AST/AST.h"

namespace Soul::AST::Visitors::UT
{
	using namespace Soul::Types;
	using namespace Soul::Parser;

	class ErrorCollectorTest : public ::testing::Test
	{
	};

	TEST_F(ErrorCollectorTest, NothingToCollect)
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
		function_declaration_statements.push_back(std::move(for_loop));
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
		auto module = ModuleNode::Create("error_collector_module", std::move(module_statements));

		ErrorCollectorVisitor error_collector{};
		error_collector.Accept(module.get());

		EXPECT_TRUE(error_collector.IsValid());
	}

	TEST_F(ErrorCollectorTest, ErrorsUpToDepth)
	{
		auto struct_declaration_members = ASTNode::Dependencies{};
		struct_declaration_members.reserve(3);
		struct_declaration_members.emplace_back(
			VariableDeclarationNode::Create("my_int", k_base_specifier_i32, nullptr, false));
		struct_declaration_members.emplace_back(
			VariableDeclarationNode::Create("my_str", k_base_specifier_str, nullptr, false));
		struct_declaration_members.emplace_back(ErrorNode::Create("struct_declaration_error"));
		auto struct_declaration = StructDeclarationNode::Create("my_struct", std::move(struct_declaration_members));

		auto if_node_true_statements = ASTNode::Dependencies{};
		if_node_true_statements.emplace_back(
			UnaryNode::Create(LiteralNode::Create(Identifier::create("index")), ASTNode::Operator::DECREMENT));
		auto if_node_false_statements = ASTNode::Dependencies{};
		if_node_false_statements.emplace_back(
			UnaryNode::Create(ErrorNode::Create("if_false_unary_expr_error"), ASTNode::Operator::INCREMENT));

		auto if_node = IfNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true)),
		                              BlockNode::Create(std::move(if_node_true_statements)),
		                              BlockNode::Create(std::move(if_node_false_statements)));

		auto for_loop_initialization = VariableDeclarationNode::Create(
			"index", k_base_specifier_i32, LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(0)), false);
		auto for_loop_condition = BinaryNode::Create(LiteralNode::Create(Identifier::create("index")),
		                                             ErrorNode::Create("for_loop_condition_binary_rhs_expr_error"),
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
		function_declaration_statements.push_back(std::move(for_loop));
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
		module_statements.push_back(ErrorNode::Create("module_error"));
		module_statements.push_back(std::move(function_declaration));
		auto module = ModuleNode::Create("error_collector_module", std::move(module_statements));

		ErrorCollectorVisitor error_collector{ 3 };
		error_collector.Accept(module.get());

		EXPECT_FALSE(error_collector.IsValid());

		using namespace std::string_view_literals;
		static constexpr std::array k_expected_errors = {
			std::make_pair(3, "struct_declaration_error"sv),
			std::make_pair(2, "module_error"sv),
		};
		const auto& result_errors = error_collector.GetErrors();
		ASSERT_EQ(result_errors.size(), k_expected_errors.size());
		for (std::size_t index = 0; index < result_errors.size(); ++index) {
			const auto& [expected_depth, expected_message] = k_expected_errors[index];
			const auto& [result_depth, error_node]         = result_errors[index];
			EXPECT_EQ(expected_depth, result_depth);
			ASSERT_TRUE(error_node);
			EXPECT_EQ(expected_message, error_node->message);
		}
	}

	TEST_F(ErrorCollectorTest, AllErrors)
	{
		auto struct_declaration_members = ASTNode::Dependencies{};
		struct_declaration_members.reserve(3);
		struct_declaration_members.emplace_back(
			VariableDeclarationNode::Create("my_int", k_base_specifier_i32, nullptr, false));
		struct_declaration_members.emplace_back(
			VariableDeclarationNode::Create("my_str", k_base_specifier_str, nullptr, false));
		struct_declaration_members.emplace_back(ErrorNode::Create("struct_declaration_error"));
		auto struct_declaration = StructDeclarationNode::Create("my_struct", std::move(struct_declaration_members));

		auto if_node_true_statements = ASTNode::Dependencies{};
		if_node_true_statements.emplace_back(
			UnaryNode::Create(LiteralNode::Create(Identifier::create("index")), ASTNode::Operator::DECREMENT));
		auto if_node_false_statements = ASTNode::Dependencies{};
		if_node_false_statements.emplace_back(
			UnaryNode::Create(ErrorNode::Create("if_false_unary_expr_error"), ASTNode::Operator::INCREMENT));

		auto if_node = IfNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true)),
		                              BlockNode::Create(std::move(if_node_true_statements)),
		                              BlockNode::Create(std::move(if_node_false_statements)));

		auto for_loop_initialization = VariableDeclarationNode::Create(
			"index", k_base_specifier_i32, LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(0)), false);
		auto for_loop_condition = BinaryNode::Create(LiteralNode::Create(Identifier::create("index")),
		                                             ErrorNode::Create("for_loop_condition_binary_rhs_expr_error"),
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
		function_declaration_statements.push_back(std::move(for_loop));
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
		module_statements.push_back(ErrorNode::Create("module_error"));
		module_statements.push_back(std::move(function_declaration));
		auto module = ModuleNode::Create("error_collector_module", std::move(module_statements));

		ErrorCollectorVisitor error_collector{};
		error_collector.Accept(module.get());

		EXPECT_FALSE(error_collector.IsValid());

		using namespace std::string_view_literals;
		static constexpr std::array k_expected_errors = {
			std::make_pair(3, "struct_declaration_error"sv),
			std::make_pair(2, "module_error"sv),
			std::make_pair(6, "for_loop_condition_binary_rhs_expr_error"sv),
			std::make_pair(9, "if_false_unary_expr_error"sv),
		};
		const auto& result_errors = error_collector.GetErrors();
		ASSERT_EQ(result_errors.size(), k_expected_errors.size());
		for (std::size_t index = 0; index < result_errors.size(); ++index) {
			const auto& [expected_depth, expected_message] = k_expected_errors[index];
			const auto& [result_depth, error_node]         = result_errors[index];
			EXPECT_EQ(expected_depth, result_depth);
			ASSERT_TRUE(error_node);
			EXPECT_EQ(expected_message, error_node->message);
		}
	}

}  // namespace Soul::AST::Visitors::UT
