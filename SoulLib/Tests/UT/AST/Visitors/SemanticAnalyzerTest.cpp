#include "AST/Visitors/SemanticAnalyzer.h"

#include <gtest/gtest.h>

#include "AST/AST.h"
#include "AST/Visitors/Compare.h"
#include "AST/Visitors/Stringify.h"
#include "AST/Visitors/TypeDiscoverer.h"
#include "AST/Visitors/TypeResolver.h"
#include "Lexer/Lexer.h"
#include "Parser/Parser.h"

#include <string_view>

namespace Soul::AST::Visitors::UT
{
	using namespace Soul::Types;

	class SemanticAnalyzerVisitorTest : public ::testing::Test
	{
		public:
		static constexpr auto k_module_name = "semantic_analyzer_module";

		protected:
		static ASTNode::Dependency Parse(std::string_view script)
		{
			auto tokens = Lexer::Lexer::Tokenize(script);
			return Parser::Parser::Parse(k_module_name, tokens);
		}

		static ASTNode::Dependency Build(ASTNode::Dependency&& root, bool do_analyze)
		{
			TypeDiscovererVisitor type_discoverer_visitor{};
			type_discoverer_visitor.Accept(root.get());
			auto type_discoverer_root = type_discoverer_visitor.Cloned();
			if (!type_discoverer_root) {
				return nullptr;
			}

			TypeResolverVisitor type_resolver_visitor{ type_discoverer_visitor.GetDiscoveredTypes() };
			type_resolver_visitor.Accept(type_discoverer_root.get());
			auto type_resolver_root = type_resolver_visitor.Cloned();
			if (!type_resolver_root) {
				return nullptr;
			}

			if (!do_analyze) {
				return type_resolver_root;
			}

			SemanticAnalyzerVisitor semantic_analyzer_visitor{};
			semantic_analyzer_visitor.Accept(type_resolver_root.get());
			auto semantic_analyzer_root = semantic_analyzer_visitor.Cloned();
			if (!semantic_analyzer_root) {
				return nullptr;
			}

			return semantic_analyzer_root;
		}
		static std::pair<std::string, std::string> Compare(const ASTNode::Reference expected,
		                                                   const ASTNode::Reference& result)
		{
			auto compare_result = CompareVisitor(expected, result);
			if (compare_result != std::partial_ordering::equivalent) {
				StringifyVisitor print_result{};
				print_result.Accept(result);

				StringifyVisitor print_expected{};
				print_expected.Accept(expected);

				return std::make_pair(std::move(print_expected.string()), std::move(print_result.string()));
			}
			return std::make_pair(std::string(), std::string());
		}
	};

	TEST_F(SemanticAnalyzerVisitorTest, VariableDeclaration_InGlobalStope)
	{
		auto result_module = Build(Parse("let my_variable : i32 = 123"), true);
		ASSERT_TRUE(result_module);

		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(
			ErrorNode::Create("variable 'my_variable' cannot be declared in the global scope."));
		auto expected_module = Build(ModuleNode::Create(k_module_name, std::move(module_statements)), false);

		auto [expected_string, result_string] = Compare(expected_module.get(), result_module.get());
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(SemanticAnalyzerVisitorTest, VariableDeclaration_AttemptedWriteToNonMutable)
	{
		auto result_module = Build(Parse("fn test_function :: void { let a: i32 = 123; a = 456; } "), true);
		ASSERT_TRUE(result_module);

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(
			VariableDeclarationNode::Create("a",
		                                    Parser::k_base_specifier_i32,
		                                    LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(123)),
		                                    false));
		function_declaration_statements.emplace_back(
			ErrorNode::Create("cannot assign to variable 'a', because it is not mutable."));
		auto function_declaration
			= FunctionDeclarationNode::Create("test_function",
		                                      Parser::k_base_specifier_void,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::Create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto expected_module = Build(ModuleNode::Create(k_module_name, std::move(module_statements)), false);

		auto [expected_string, result_string] = Compare(expected_module.get(), result_module.get());
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(SemanticAnalyzerVisitorTest, LiteralNode_UndeclaredIndentifier)
	{
		auto result_module = Build(Parse("a + b"), true);
		ASSERT_TRUE(result_module);

		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(BinaryNode::Create(ErrorNode::Create("use of undeclared identifier 'a'"),
		                                                  ErrorNode::Create("use of undeclared identifier 'b'"),
		                                                  ASTNode::Operator::ADD));
		auto expected_module = Build(ModuleNode::Create(k_module_name, std::move(module_statements)), false);

		auto [expected_string, result_string] = Compare(expected_module.get(), result_module.get());
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(SemanticAnalyzerVisitorTest, LoopControlNode_OutsideOfLoop)
	{
		auto result_module = Build(Parse("break"), true);
		ASSERT_TRUE(result_module);

		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(ErrorNode::Create("keyword 'break' must be used in a loop context."));
		auto expected_module = Build(ModuleNode::Create(k_module_name, std::move(module_statements)), false);

		auto [expected_string, result_string] = Compare(expected_module.get(), result_module.get());
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(SemanticAnalyzerVisitorTest, LoopControlNode_InsideLoopsBody)
	{
		auto result_module = Build(Parse("for (;;) { continue; }"), true);
		ASSERT_TRUE(result_module);

		auto for_loop_statements = ASTNode::Dependencies{};
		for_loop_statements.emplace_back(LoopControlNode::Create(LoopControlNode::Type::CONTINUE));
		auto for_loop
			= ForLoopNode::Create(nullptr, nullptr, nullptr, BlockNode::Create(std::move(for_loop_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(for_loop));
		auto expected_module = Build(ModuleNode::Create(k_module_name, std::move(module_statements)), false);

		auto [expected_string, result_string] = Compare(expected_module.get(), result_module.get());
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(SemanticAnalyzerVisitorTest, LoopControlNode_InNestedLoop)
	{
		auto result_module
			= Build(Parse("for (;;) { for (;;) { # This shouldn't throw off the analyzer\n } break; }"), true);
		ASSERT_TRUE(result_module);

		auto inner_for_loop_statements = ASTNode::Dependencies{};
		auto inner_for_loop
			= ForLoopNode::Create(nullptr, nullptr, nullptr, BlockNode::Create(std::move(inner_for_loop_statements)));

		auto outer_for_loop_statements = ASTNode::Dependencies{};
		outer_for_loop_statements.push_back(std::move(inner_for_loop));
		outer_for_loop_statements.emplace_back(LoopControlNode::Create(LoopControlNode::Type::BREAK));
		auto outer_for_loop
			= ForLoopNode::Create(nullptr, nullptr, nullptr, BlockNode::Create(std::move(outer_for_loop_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(outer_for_loop));
		auto expected_module = Build(ModuleNode::Create(k_module_name, std::move(module_statements)), false);

		auto [expected_string, result_string] = Compare(expected_module.get(), result_module.get());
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(SemanticAnalyzerVisitorTest, FunctionDeclaration_ImplicitVoidReturn)
	{
		auto result_module = Build(Parse(""), true);
		ASSERT_TRUE(result_module);

		auto module_statements = ASTNode::Dependencies{};
		auto expected_module   = Build(ModuleNode::Create(k_module_name, std::move(module_statements)), false);

		auto [expected_string, result_string] = Compare(expected_module.get(), result_module.get());
		ASSERT_EQ(expected_string, result_string);
	}
}  // namespace Soul::AST::Visitors::UT
