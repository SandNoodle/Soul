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

namespace soul::ast::visitors::ut
{
	using namespace soul::types;
	using namespace soul::parser;

	class SemanticAnalyzerVisitorTest : public ::testing::Test
	{
		public:
		static constexpr auto k_module_name = "semantic_analyzer_module";

		protected:
		ASTNode::Dependency parse(std::string_view script)
		{
			auto tokens = lexer::Lexer::Tokenize(script);
			return parser::Parser::parse(k_module_name, tokens);
		}

		ASTNode::Dependency build(ASTNode::Dependency&& root, bool do_analyze)
		{
			TypeDiscovererVisitor type_discoverer_visitor{};
			type_discoverer_visitor.accept(root.get());
			auto type_discoverer_root = type_discoverer_visitor.cloned();
			if (!type_discoverer_root) {
				return nullptr;
			}

			TypeResolverVisitor type_resolver_visitor{ type_discoverer_visitor.discovered_types() };
			type_resolver_visitor.accept(type_discoverer_root.get());
			auto type_resolver_root = type_resolver_visitor.cloned();
			if (!type_resolver_root) {
				return nullptr;
			}

			if (!do_analyze) {
				return type_resolver_root;
			}

			SemanticAnalyzerVisitor semantic_analyzer_visitor{};
			semantic_analyzer_visitor.accept(type_resolver_root.get());
			auto semantic_analyzer_root = semantic_analyzer_visitor.cloned();
			if (!semantic_analyzer_root) {
				return nullptr;
			}

			return semantic_analyzer_root;
		}
		std::pair<std::string, std::string> compare(const ASTNode::Reference expected, const ASTNode::Reference& result)
		{
			auto compare_result = CompareVisitor(expected, result);
			if (compare_result != std::partial_ordering::equivalent) {
				StringifyVisitor print_result{};
				print_result.accept(result);

				StringifyVisitor print_expected{};
				print_expected.accept(expected);

				return std::make_pair(std::move(print_expected.string()), std::move(print_result.string()));
			}
			return std::make_pair(std::string(), std::string());
		}
	};

	TEST_F(SemanticAnalyzerVisitorTest, VariableDeclaration_InGlobalStope)
	{
		auto result_module = build(parse("let my_variable : i32 = 123"), true);
		ASSERT_TRUE(result_module);

		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(
			ErrorNode::create("variable 'my_variable' cannot be declared in the global scope."));
		auto expected_module = build(ModuleNode::create(k_module_name, std::move(module_statements)), false);

		auto [expected_string, result_string] = compare(expected_module.get(), result_module.get());
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(SemanticAnalyzerVisitorTest, VariableDeclaration_AttemptedWriteToNonMutable)
	{
		auto result_module = build(parse("fn test_function :: void { let a: i32 = 123; a = 456; } "), true);
		ASSERT_TRUE(result_module);

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(VariableDeclarationNode::create(
			"a", k_base_specifier_i32, LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(123)), false));
		function_declaration_statements.emplace_back(
			ErrorNode::create("cannot assign to variable 'a', because it is not mutable."));
		auto function_declaration
			= FunctionDeclarationNode::create("test_function",
		                                      k_base_specifier_void,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto expected_module = build(ModuleNode::create(k_module_name, std::move(module_statements)), false);

		auto [expected_string, result_string] = compare(expected_module.get(), result_module.get());
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(SemanticAnalyzerVisitorTest, LiteralNode_UndeclaredIndentifier)
	{
		auto result_module = build(parse("a + b"), true);
		ASSERT_TRUE(result_module);

		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(BinaryNode::create(ErrorNode::create("use of undeclared identifier 'a'"),
		                                                  ErrorNode::create("use of undeclared identifier 'b'"),
		                                                  ASTNode::Operator::Add));
		auto expected_module = build(ModuleNode::create(k_module_name, std::move(module_statements)), false);

		auto [expected_string, result_string] = compare(expected_module.get(), result_module.get());
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(SemanticAnalyzerVisitorTest, LoopControlNode_OutsideOfLoop)
	{
		auto result_module = build(parse("break"), true);
		ASSERT_TRUE(result_module);

		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(ErrorNode::create("keyword 'break' must be used in a loop context."));
		auto expected_module = build(ModuleNode::create(k_module_name, std::move(module_statements)), false);

		auto [expected_string, result_string] = compare(expected_module.get(), result_module.get());
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(SemanticAnalyzerVisitorTest, LoopControlNode_InsideLoopsBody)
	{
		auto result_module = build(parse("for (;;) { continue; }"), true);
		ASSERT_TRUE(result_module);

		auto for_loop_statements = ASTNode::Dependencies{};
		for_loop_statements.emplace_back(LoopControlNode::create(LoopControlNode::Type::Continue));
		auto for_loop
			= ForLoopNode::create(nullptr, nullptr, nullptr, BlockNode::create(std::move(for_loop_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(for_loop));
		auto expected_module = build(ModuleNode::create(k_module_name, std::move(module_statements)), false);

		auto [expected_string, result_string] = compare(expected_module.get(), result_module.get());
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(SemanticAnalyzerVisitorTest, LoopControlNode_InNestedLoop)
	{
		auto result_module
			= build(parse("for (;;) { for (;;) { # This shouldn't throw off the analyzer\n } break; }"), true);
		ASSERT_TRUE(result_module);

		auto inner_for_loop_statements = ASTNode::Dependencies{};
		auto inner_for_loop
			= ForLoopNode::create(nullptr, nullptr, nullptr, BlockNode::create(std::move(inner_for_loop_statements)));

		auto outer_for_loop_statements = ASTNode::Dependencies{};
		outer_for_loop_statements.push_back(std::move(inner_for_loop));
		outer_for_loop_statements.emplace_back(LoopControlNode::create(LoopControlNode::Type::Break));
		auto outer_for_loop
			= ForLoopNode::create(nullptr, nullptr, nullptr, BlockNode::create(std::move(outer_for_loop_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(outer_for_loop));
		auto expected_module = build(ModuleNode::create(k_module_name, std::move(module_statements)), false);

		auto [expected_string, result_string] = compare(expected_module.get(), result_module.get());
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(SemanticAnalyzerVisitorTest, FunctionDeclaration_ImplicitVoidReturn)
	{
		auto result_module = build(parse(""), true);
		ASSERT_TRUE(result_module);

		auto module_statements = ASTNode::Dependencies{};
		auto expected_module   = build(ModuleNode::create(k_module_name, std::move(module_statements)), false);

		auto [expected_string, result_string] = compare(expected_module.get(), result_module.get());
		ASSERT_EQ(expected_string, result_string);
	}
}  // namespace soul::ast::visitors::ut
