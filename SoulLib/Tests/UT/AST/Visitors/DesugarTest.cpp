#include "AST/Visitors/Desugar.h"

#include <gtest/gtest.h>

#include "AST/AST.h"
#include "AST/Visitors/Compare.h"
#include "AST/Visitors/ErrorCollector.h"
#include "AST/Visitors/Stringify.h"
#include "AST/Visitors/TypeDiscoverer.h"
#include "AST/Visitors/TypeResolver.h"

namespace Soul::AST::Visitors::UT
{
	using namespace Soul::Types;
	using namespace Soul::Parser;

	class DesugarVisitorTest : public ::testing::Test
	{
		public:
		static constexpr auto k_module_name = "desugar_module";

		protected:
		ASTNode::Dependency build(ASTNode::Dependency&& root)
		{
			TypeDiscovererVisitor type_discoverer_visitor{};
			type_discoverer_visitor.Accept(root.get());

			auto type_discoverer_root = type_discoverer_visitor.Cloned();
			if (type_discoverer_visitor.Affects()) {
				ErrorCollectorVisitor error_collector{};
				error_collector.Accept(type_discoverer_root.get());
				if (!error_collector.IsValid()) {
					for (const auto& [depth, error] : error_collector.GetErrors()) {
						std::cerr << std::format("[{}]: {}\n", depth, error->message);
					}
					return nullptr;
				}
			}

			TypeResolverVisitor type_resolver_visitor{ type_discoverer_visitor.GetDiscoveredTypes() };
			type_resolver_visitor.Accept(type_discoverer_root.get());

			auto type_resolver_root = type_resolver_visitor.Cloned();
			if (type_resolver_visitor.Affects()) {
				ErrorCollectorVisitor error_collector{};
				error_collector.Accept(type_resolver_root.get());
				if (!error_collector.IsValid()) {
					for (const auto& [depth, error] : error_collector.GetErrors()) {
						std::cerr << std::format("[{}]: {}\n", depth, error->message);
					}
					return nullptr;
				}
			}

			DesugarVisitor desugar_visitor{};
			desugar_visitor.Accept(type_resolver_root.get());

			return desugar_visitor.Cloned();
		}

		// NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
		void verify(const ASTNode::Reference expected, const ASTNode::Reference result)
		{
			if (CompareVisitor{ expected, result }) [[likely]] {
				return;
			}

			StringifyVisitor stringify_expected{ StringifyVisitor::Options::PRINT_TYPES };
			stringify_expected.Accept(expected);

			StringifyVisitor stringify_result{ StringifyVisitor::Options::PRINT_TYPES };
			stringify_result.Accept(result);

			GTEST_FAIL() << "Expected:\n"
						 << stringify_expected.string() << "\n...but got:\n"
						 << stringify_result.string();
		}
	};

	TEST_F(DesugarVisitorTest, Binary)
	{
		// Get complex assignment operators...
		static constexpr std::array k_substitutable_operators = { ASTNode::Operator::ASSIGN_ADD,
			                                                      ASTNode::Operator::ASSIGN_SUB,
			                                                      ASTNode::Operator::ASSIGN_MUL,
			                                                      ASTNode::Operator::ASSIGN_DIV,
			                                                      ASTNode::Operator::ASSIGN_MOD };

		auto input_module_statements = ASTNode::Dependencies{};
		input_module_statements.reserve(k_substitutable_operators.size());
		for (const auto op : k_substitutable_operators) {
			input_module_statements.emplace_back(
				BinaryNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(1)),
			                       LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(2)),
			                       op));
		}
		// ...desugar them...
		auto result_module = build(ModuleNode::Create(k_module_name, std::move(input_module_statements)));
		ASSERT_TRUE(result_module);

		// ...and verify the results.
		static constexpr std::array k_result_operators = { ASTNode::Operator::ADD,
			                                               ASTNode::Operator::SUB,
			                                               ASTNode::Operator::MUL,
			                                               ASTNode::Operator::DIV,
			                                               ASTNode::Operator::MOD };

		auto expected_module_statements = ASTNode::Dependencies{};
		expected_module_statements.reserve(k_result_operators.size());
		for (const auto op : k_result_operators) {
			expected_module_statements.emplace_back(BinaryNode::Create(
				LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(1)),
				BinaryNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(1)),
			                       LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(2)),
			                       op),
				ASTNode::Operator::ASSIGN));
		}
		auto expected_module = build(ModuleNode::Create(k_module_name, std::move(expected_module_statements)));
		ASSERT_TRUE(expected_module);

		verify(expected_module.get(), result_module.get());
	}

	TEST_F(DesugarVisitorTest, ForLoop)
	{
		// Get a ForLoop node...
		auto for_loop_initialization = VariableDeclarationNode::Create(
			"index", k_base_specifier_i32, LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(0)), true);

		auto for_loop_condition
			= BinaryNode::Create(LiteralNode::Create(Identifier::create("index")),
		                         LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(10)),
		                         ASTNode::Operator::LESS);

		auto for_loop_update
			= UnaryNode::Create(LiteralNode::Create(Identifier::create("index")), ASTNode::Operator::INCREMENT);

		auto for_loop_statements = ASTNode::Dependencies{};
		for_loop_statements.push_back(
			VariableDeclarationNode::Create("inner",
		                                    k_base_specifier_f32,
		                                    LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::FLOAT32>(3.14)),
		                                    false));

		auto for_loop = ForLoopNode::Create(std::move(for_loop_initialization),
		                                    std::move(for_loop_condition),
		                                    std::move(for_loop_update),
		                                    BlockNode::Create(std::move(for_loop_statements)));

		auto input_module_statements = ASTNode::Dependencies{};
		input_module_statements.push_back(std::move(for_loop));

		// ...desugar it...
		auto result_module = build(ModuleNode::Create(k_module_name, std::move(input_module_statements)));
		ASSERT_TRUE(result_module);

		// ...and verify the results.
		auto while_node_initialization = VariableDeclarationNode::Create(
			"index", k_base_specifier_i32, LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(0)), true);

		auto while_node_condition
			= BinaryNode::Create(LiteralNode::Create(Identifier::create("index")),
		                         LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(10)),
		                         ASTNode::Operator::LESS);

		auto while_node_statements = ASTNode::Dependencies{};
		while_node_statements.reserve(2);
		while_node_statements.push_back(
			VariableDeclarationNode::Create("inner",
		                                    k_base_specifier_f32,
		                                    LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::FLOAT32>(3.14)),
		                                    false));
		while_node_statements.push_back(
			UnaryNode::Create(LiteralNode::Create(Identifier::create("index")), ASTNode::Operator::INCREMENT));

		auto while_node
			= WhileNode::Create(std::move(while_node_condition), BlockNode::Create(std::move(while_node_statements)));

		auto outer_statements = ASTNode::Dependencies{};
		outer_statements.reserve(2);
		outer_statements.push_back(std::move(while_node_initialization));
		outer_statements.push_back(std::move(while_node));

		auto expected_module_statements = ASTNode::Dependencies{};
		expected_module_statements.push_back(BlockNode::Create(std::move(outer_statements)));
		auto expected_module = build(ModuleNode::Create(k_module_name, std::move(expected_module_statements)));
		ASSERT_TRUE(expected_module);

		verify(expected_module.get(), result_module.get());
	}
}  // namespace Soul::AST::Visitors::UT
