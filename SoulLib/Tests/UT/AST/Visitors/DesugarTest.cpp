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
			type_discoverer_visitor.accept(root.get());

			auto type_discoverer_root = type_discoverer_visitor.cloned();
			if (type_discoverer_visitor.affects()) {
				ErrorCollectorVisitor error_collector{};
				error_collector.accept(type_discoverer_root.get());
				if (!error_collector.is_valid()) {
					for (const auto& [depth, error] : error_collector.errors()) {
						std::cerr << std::format("[{}]: {}\n", depth, error->message);
					}
					return nullptr;
				}
			}

			TypeResolverVisitor type_resolver_visitor{ type_discoverer_visitor.discovered_types() };
			type_resolver_visitor.accept(type_discoverer_root.get());

			auto type_resolver_root = type_resolver_visitor.cloned();
			if (type_resolver_visitor.affects()) {
				ErrorCollectorVisitor error_collector{};
				error_collector.accept(type_resolver_root.get());
				if (!error_collector.is_valid()) {
					for (const auto& [depth, error] : error_collector.errors()) {
						std::cerr << std::format("[{}]: {}\n", depth, error->message);
					}
					return nullptr;
				}
			}

			DesugarVisitor desugar_visitor{};
			desugar_visitor.accept(type_resolver_root.get());

			return desugar_visitor.cloned();
		}

		// NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
		void verify(const ASTNode::Reference expected, const ASTNode::Reference result)
		{
			if (CompareVisitor{ expected, result }) [[likely]] {
				return;
			}

			StringifyVisitor stringify_expected{ StringifyVisitor::Options::PrintTypes };
			stringify_expected.accept(expected);

			StringifyVisitor stringify_result{ StringifyVisitor::Options::PrintTypes };
			stringify_result.accept(result);

			GTEST_FAIL() << "Expected:\n"
						 << stringify_expected.string() << "\n...but got:\n"
						 << stringify_result.string();
		}
	};

	TEST_F(DesugarVisitorTest, Binary)
	{
		// Get complex assignment operators...
		static constexpr std::array k_substitutable_operators = { ASTNode::Operator::AddAssign,
			                                                      ASTNode::Operator::SubAssign,
			                                                      ASTNode::Operator::MulAssign,
			                                                      ASTNode::Operator::DivAssign,
			                                                      ASTNode::Operator::ModAssign };

		auto input_module_statements = ASTNode::Dependencies{};
		input_module_statements.reserve(k_substitutable_operators.size());
		for (const auto op : k_substitutable_operators) {
			input_module_statements.emplace_back(
				BinaryNode::create(LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(1)),
			                       LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(2)),
			                       op));
		}
		// ...desugar them...
		auto result_module = build(ModuleNode::create(k_module_name, std::move(input_module_statements)));
		ASSERT_TRUE(result_module);

		// ...and verify the results.
		static constexpr std::array k_result_operators = { ASTNode::Operator::Add,
			                                               ASTNode::Operator::Sub,
			                                               ASTNode::Operator::Mul,
			                                               ASTNode::Operator::Div,
			                                               ASTNode::Operator::Mod };

		auto expected_module_statements = ASTNode::Dependencies{};
		expected_module_statements.reserve(k_result_operators.size());
		for (const auto op : k_result_operators) {
			expected_module_statements.emplace_back(BinaryNode::create(
				LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(1)),
				BinaryNode::create(LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(1)),
			                       LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(2)),
			                       op),
				ASTNode::Operator::Assign));
		}
		auto expected_module = build(ModuleNode::create(k_module_name, std::move(expected_module_statements)));
		ASSERT_TRUE(expected_module);

		verify(expected_module.get(), result_module.get());
	}

	TEST_F(DesugarVisitorTest, ForLoop)
	{
		// Get a ForLoop node...
		auto for_loop_initialization = VariableDeclarationNode::create(
			"index", k_base_specifier_i32, LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(0)), true);

		auto for_loop_condition
			= BinaryNode::create(LiteralNode::create(Identifier::create("index")),
		                         LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(10)),
		                         ASTNode::Operator::Less);

		auto for_loop_update
			= UnaryNode::create(LiteralNode::create(Identifier::create("index")), ASTNode::Operator::Increment);

		auto for_loop_statements = ASTNode::Dependencies{};
		for_loop_statements.push_back(
			VariableDeclarationNode::create("inner",
		                                    k_base_specifier_f32,
		                                    LiteralNode::create(Scalar::create<PrimitiveType::Kind::Float32>(3.14)),
		                                    false));

		auto for_loop = ForLoopNode::create(std::move(for_loop_initialization),
		                                    std::move(for_loop_condition),
		                                    std::move(for_loop_update),
		                                    BlockNode::create(std::move(for_loop_statements)));

		auto input_module_statements = ASTNode::Dependencies{};
		input_module_statements.push_back(std::move(for_loop));

		// ...desugar it...
		auto result_module = build(ModuleNode::create(k_module_name, std::move(input_module_statements)));
		ASSERT_TRUE(result_module);

		// ...and verify the results.
		auto while_node_initialization = VariableDeclarationNode::create(
			"index", k_base_specifier_i32, LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(0)), true);

		auto while_node_condition
			= BinaryNode::create(LiteralNode::create(Identifier::create("index")),
		                         LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(10)),
		                         ASTNode::Operator::Less);

		auto while_node_statements = ASTNode::Dependencies{};
		while_node_statements.reserve(2);
		while_node_statements.push_back(
			VariableDeclarationNode::create("inner",
		                                    k_base_specifier_f32,
		                                    LiteralNode::create(Scalar::create<PrimitiveType::Kind::Float32>(3.14)),
		                                    false));
		while_node_statements.push_back(
			UnaryNode::create(LiteralNode::create(Identifier::create("index")), ASTNode::Operator::Increment));

		auto while_node
			= WhileNode::create(std::move(while_node_condition), BlockNode::create(std::move(while_node_statements)));

		auto outer_statements = ASTNode::Dependencies{};
		outer_statements.reserve(2);
		outer_statements.push_back(std::move(while_node_initialization));
		outer_statements.push_back(std::move(while_node));

		auto expected_module_statements = ASTNode::Dependencies{};
		expected_module_statements.push_back(BlockNode::create(std::move(outer_statements)));
		auto expected_module = build(ModuleNode::create(k_module_name, std::move(expected_module_statements)));
		ASSERT_TRUE(expected_module);

		verify(expected_module.get(), result_module.get());
	}
}  // namespace Soul::AST::Visitors::UT
