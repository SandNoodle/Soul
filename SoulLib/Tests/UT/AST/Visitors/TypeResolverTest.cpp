#include "AST/Visitors/TypeResolver.h"

#include <gtest/gtest.h>

#include "AST/AST.h"
#include "AST/Visitors/ErrorCollector.h"

#include <format>
#include <string_view>

namespace Soul::AST::Visitors::UT
{
	using namespace Soul::Types;
	using namespace Soul::Parser;

	class TypeResolverTest : public ::testing::Test
	{
		public:
		static constexpr auto k_module_name = "resolve_module";

		protected:
		ASTNode::Dependency resolve(ASTNode::Reference root)
		{
			TypeDiscovererVisitor type_discoverer_visitor{};
			type_discoverer_visitor.Accept(root);

			auto type_discoverer_root = type_discoverer_visitor.Cloned();

			ErrorCollectorVisitor error_collector{};
			error_collector.Accept(type_discoverer_root.get());
			if (!error_collector.IsValid()) {
				for (const auto& [depth, error] : error_collector.GetErrors()) {
					std::cerr << std::format("[{}]: {}\n", depth, error->message);
				}
				return nullptr;
			}

			TypeResolverVisitor type_resolver_visitor{ type_discoverer_visitor.GetDiscoveredTypes() };
			type_resolver_visitor.Accept(type_discoverer_root.get());

			return type_resolver_visitor.Cloned();
		}
	};

	TEST_F(TypeResolverTest, BinaryNode_Arithmetic)
	{
		static constexpr std::array k_arithmetic_operators = { ASTNode::Operator::ADD,
			                                                   ASTNode::Operator::SUB,
			                                                   ASTNode::Operator::MUL,
			                                                   ASTNode::Operator::DIV,
			                                                   ASTNode::Operator::MOD };

		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(k_arithmetic_operators.size());
		for (const auto op : k_arithmetic_operators) {
			module_statements.emplace_back(
				BinaryNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT64>(1L)),
			                       LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT64>(5L)),
			                       op));
		}
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), k_arithmetic_operators.size());

		for (std::size_t index = 0; index < k_arithmetic_operators.size(); ++index) {
			ASSERT_TRUE(as_module.statements[index]->Is<BinaryNode>());
			const auto& as_binary = as_module.statements[index]->As<BinaryNode>();
			EXPECT_EQ(as_binary.op, k_arithmetic_operators[index]);
			EXPECT_EQ(as_binary.type, PrimitiveType::Kind::INT64);

			ASSERT_TRUE(as_binary.lhs->Is<LiteralNode>());
			const auto& as_lhs = as_binary.lhs->As<LiteralNode>();
			EXPECT_EQ(as_lhs.value, Scalar::Create<PrimitiveType::Kind::INT64>(1L));
			EXPECT_EQ(as_lhs.type, PrimitiveType::Kind::INT64);

			ASSERT_TRUE(as_binary.rhs->Is<LiteralNode>());
			const auto& as_rhs = as_binary.rhs->As<LiteralNode>();
			EXPECT_EQ(as_rhs.value, Scalar::Create<PrimitiveType::Kind::INT64>(5L));
			EXPECT_EQ(as_rhs.type, PrimitiveType::Kind::INT64);
		}
	}

	TEST_F(TypeResolverTest, BinaryNode_Comparison)
	{
		static constexpr std::array k_comparison_operators

			= { ASTNode::Operator::EQUAL,         ASTNode::Operator::NOT_EQUAL, ASTNode::Operator::GREATER,
			    ASTNode::Operator::GREATER_EQUAL, ASTNode::Operator::LESS,      ASTNode::Operator::LESS_EQUAL };

		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(k_comparison_operators.size());
		for (const auto op : k_comparison_operators) {
			module_statements.emplace_back(
				BinaryNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::FLOAT64>(1.0)),
			                       LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::FLOAT64>(5.0)),
			                       op));
		}
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), k_comparison_operators.size());

		for (std::size_t index = 0; index < k_comparison_operators.size(); ++index) {
			ASSERT_TRUE(as_module.statements[index]->Is<BinaryNode>());
			const auto& as_binary = as_module.statements[index]->As<BinaryNode>();
			EXPECT_EQ(as_binary.op, k_comparison_operators[index]);
			EXPECT_EQ(as_binary.type, PrimitiveType::Kind::BOOLEAN);

			ASSERT_TRUE(as_binary.lhs->Is<LiteralNode>());
			const auto& as_lhs = as_binary.lhs->As<LiteralNode>();
			EXPECT_EQ(as_lhs.value, Scalar::Create<PrimitiveType::Kind::FLOAT64>(1.0));
			EXPECT_EQ(as_lhs.type, PrimitiveType::Kind::FLOAT64);

			ASSERT_TRUE(as_binary.rhs->Is<LiteralNode>());
			const auto& as_rhs = as_binary.rhs->As<LiteralNode>();
			EXPECT_EQ(as_rhs.value, Scalar::Create<PrimitiveType::Kind::FLOAT64>(5.0));
			EXPECT_EQ(as_rhs.type, PrimitiveType::Kind::FLOAT64);
		}
	}

	TEST_F(TypeResolverTest, BinaryNode_Logical)
	{
		static constexpr std::array k_logical_operators
			= { ASTNode::Operator::LOGICAL_OR, ASTNode::Operator::LOGICAL_AND };

		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(k_logical_operators.size());
		for (const auto op : k_logical_operators) {
			module_statements.emplace_back(
				BinaryNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true)),
			                       LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(false)),
			                       op));
		}
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), k_logical_operators.size());

		for (std::size_t index = 0; index < k_logical_operators.size(); ++index) {
			ASSERT_TRUE(as_module.statements[index]->Is<BinaryNode>());
			const auto& as_binary = as_module.statements[index]->As<BinaryNode>();
			EXPECT_EQ(as_binary.op, k_logical_operators[index]);
			EXPECT_EQ(as_binary.type, PrimitiveType::Kind::BOOLEAN);

			ASSERT_TRUE(as_binary.lhs->Is<LiteralNode>());
			const auto& as_lhs = as_binary.lhs->As<LiteralNode>();
			EXPECT_EQ(as_lhs.value, Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true));
			EXPECT_EQ(as_lhs.type, PrimitiveType::Kind::BOOLEAN);

			ASSERT_TRUE(as_binary.rhs->Is<LiteralNode>());
			const auto& as_rhs = as_binary.rhs->As<LiteralNode>();
			EXPECT_EQ(as_rhs.value, Scalar::Create<PrimitiveType::Kind::BOOLEAN>(false));
			EXPECT_EQ(as_rhs.type, PrimitiveType::Kind::BOOLEAN);
		}
	}

	TEST_F(TypeResolverTest, BinaryNode_NoOverload)
	{
		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(
			BinaryNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::STRING>("my_string")),
		                       LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::FLOAT32>(5)),
		                       ASTNode::Operator::LOGICAL_AND));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 1);

		ASSERT_TRUE(as_module.statements[0]->Is<ErrorNode>());
		const auto& as_error = as_module.statements[0]->As<ErrorNode>();
		EXPECT_EQ(as_error.message,
		          std::format("operator ('{}') does not exist for types '{}' and '{}'",
		                      ASTNode::Name(ASTNode::Operator::LOGICAL_AND),
		                      std::string(Type{ PrimitiveType::Kind::STRING }),
		                      std::string(Type{ PrimitiveType::Kind::FLOAT32 })));
	}

	TEST_F(TypeResolverTest, BlockNode)
	{
		auto block_node_statements = ASTNode::Dependencies{};
		block_node_statements.emplace_back(VariableDeclarationNode::Create(
			"in_scope", k_base_specifier_f32, LiteralNode::Create(Identifier::create("before_scope")), false));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(3);
		module_statements.emplace_back(
			VariableDeclarationNode::Create("before_scope",
		                                    k_base_specifier_f32,
		                                    LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::FLOAT32>(1.0)),
		                                    false));
		module_statements.emplace_back(BlockNode::Create(std::move(block_node_statements)));
		module_statements.emplace_back(
			VariableDeclarationNode::Create("after_scope",
		                                    k_base_specifier_i32,
		                                    LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(5)),
		                                    false));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 3);

		ASSERT_TRUE(as_module.statements[0]->Is<VariableDeclarationNode>());
		const auto& as_before_scope_variable = as_module.statements[0]->As<VariableDeclarationNode>();
		EXPECT_EQ(as_before_scope_variable.name, "before_scope");
		EXPECT_EQ(as_before_scope_variable.type_specifier, k_base_specifier_f32);
		EXPECT_TRUE(as_before_scope_variable.expression);
		{
			ASSERT_TRUE(as_before_scope_variable.expression->Is<LiteralNode>());
			const auto& as_literal = as_before_scope_variable.expression->As<LiteralNode>();
			EXPECT_EQ(as_literal.value, Scalar::Create<PrimitiveType::Kind::FLOAT32>(1.0));
			EXPECT_EQ(as_literal.type, PrimitiveType::Kind::FLOAT32);
		}
		EXPECT_FALSE(as_before_scope_variable.is_mutable);
		EXPECT_EQ(as_before_scope_variable.type, PrimitiveType::Kind::FLOAT32);

		ASSERT_TRUE(as_module.statements[1]->Is<BlockNode>());
		const auto& as_block = as_module.statements[1]->As<BlockNode>();
		ASSERT_EQ(as_block.statements.size(), 1);
		EXPECT_EQ(as_block.type, PrimitiveType::Kind::VOID);
		{
			ASSERT_TRUE(as_block.statements[0]->Is<VariableDeclarationNode>());
			const auto& as_in_scope_variable = as_block.statements[0]->As<VariableDeclarationNode>();
			EXPECT_EQ(as_in_scope_variable.name, "in_scope");
			EXPECT_EQ(as_in_scope_variable.type_specifier, k_base_specifier_f32);
			EXPECT_TRUE(as_in_scope_variable.expression);
			{
				ASSERT_TRUE(as_in_scope_variable.expression->Is<LiteralNode>());
				const auto& as_literal = as_in_scope_variable.expression->As<LiteralNode>();
				EXPECT_EQ(as_literal.value, Identifier::create("before_scope"));
				EXPECT_EQ(as_literal.type, PrimitiveType::Kind::FLOAT32);
			}
			EXPECT_FALSE(as_in_scope_variable.is_mutable);
			EXPECT_EQ(as_in_scope_variable.type, PrimitiveType::Kind::FLOAT32);
		}

		ASSERT_TRUE(as_module.statements[2]->Is<VariableDeclarationNode>());
		const auto& as_after_scope_variable = as_module.statements[2]->As<VariableDeclarationNode>();
		EXPECT_EQ(as_after_scope_variable.name, "after_scope");
		EXPECT_EQ(as_after_scope_variable.type_specifier, k_base_specifier_i32);
		EXPECT_TRUE(as_after_scope_variable.expression);
		{
			ASSERT_TRUE(as_after_scope_variable.expression->Is<LiteralNode>());
			const auto& as_literal = as_after_scope_variable.expression->As<LiteralNode>();
			EXPECT_EQ(as_literal.value, Scalar::Create<PrimitiveType::Kind::INT32>(5));
			EXPECT_EQ(as_literal.type, PrimitiveType::Kind::INT32);
		}
		EXPECT_FALSE(as_after_scope_variable.is_mutable);
		EXPECT_EQ(as_after_scope_variable.type, PrimitiveType::Kind::INT32);
	}

	TEST_F(TypeResolverTest, Cast_BasicType)
	{
		auto cast_node         = CastNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT64>(128L)),
                                          k_base_specifier_i32);
		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(cast_node));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 1);

		ASSERT_TRUE(as_module.statements[0]->Is<CastNode>());
		const auto& as_cast = as_module.statements[0]->As<CastNode>();
		EXPECT_EQ(as_cast.type_specifier, k_base_specifier_i32);
		EXPECT_EQ(as_cast.type, PrimitiveType::Kind::INT32);

		ASSERT_TRUE(as_cast.expression->Is<LiteralNode>());
		const auto& as_literal = as_cast.expression->As<LiteralNode>();
		EXPECT_EQ(as_literal.value, Scalar::Create<PrimitiveType::Kind::INT64>(128L));
		EXPECT_EQ(as_literal.type, PrimitiveType::Kind::INT64);
	}

	TEST_F(TypeResolverTest, Cast_Impossible)
	{
		auto cast_node         = CastNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT64>(128L)),
                                          k_base_specifier_chr);
		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(cast_node));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 1);

		ASSERT_TRUE(as_module.statements[0]->Is<ErrorNode>());
		const auto& as_error = as_module.statements[0]->As<ErrorNode>();
		EXPECT_EQ(as_error.message,
		          std::format("cannot cast from type '{}' to '{}'",
		                      std::string(Type{ PrimitiveType::Kind::INT64 }),
		                      std::string(Type{ PrimitiveType::Kind::CHAR })));
	}

	TEST_F(TypeResolverTest, ForLoop)
	{
		auto initialization = VariableDeclarationNode::Create(
			"index", k_base_specifier_i32, LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(0)), true);

		auto condition = BinaryNode::Create(LiteralNode::Create(Identifier::create("index")),
		                                    LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(10)),
		                                    ASTNode::Operator::LESS);

		auto update = UnaryNode::Create(LiteralNode::Create(Identifier::create("index")), ASTNode::Operator::INCREMENT);

		auto for_loop_statements = ASTNode::Dependencies{};
		for_loop_statements.emplace_back(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::STRING>("my_string")));

		auto for_loop          = ForLoopNode::Create(std::move(initialization),
                                            std::move(condition),
                                            std::move(update),
                                            BlockNode::Create(std::move(for_loop_statements)));
		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(for_loop));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 1);

		ASSERT_TRUE(as_module.statements[0]->Is<ForLoopNode>());
		const auto& as_for_loop = as_module.statements[0]->As<ForLoopNode>();

		ASSERT_TRUE(as_for_loop.initialization->Is<VariableDeclarationNode>());
		const auto& as_initialization = as_for_loop.initialization->As<VariableDeclarationNode>();
		EXPECT_EQ(as_initialization.type, PrimitiveType::Kind::INT32);
		EXPECT_EQ(as_initialization.name, "index");
		EXPECT_EQ(as_initialization.type_specifier, k_base_specifier_i32);
		EXPECT_TRUE(as_initialization.is_mutable);
		ASSERT_TRUE(as_initialization.expression);
		{
			ASSERT_TRUE(as_initialization.expression->Is<LiteralNode>());
			const auto& as_value = as_initialization.expression->As<LiteralNode>();
			EXPECT_EQ(as_value.type, PrimitiveType::Kind::INT32);
			EXPECT_EQ(as_value.value, Scalar::Create<PrimitiveType::Kind::INT32>(0));
		}

		ASSERT_TRUE(as_for_loop.condition->Is<BinaryNode>());
		const auto& as_condition = as_for_loop.condition->As<BinaryNode>();
		EXPECT_EQ(as_condition.type, PrimitiveType::Kind::BOOLEAN);
		{
			EXPECT_EQ(as_condition.op, ASTNode::Operator::LESS);

			ASSERT_TRUE(as_condition.lhs->Is<LiteralNode>());
			const auto& as_lhs = as_condition.lhs->As<LiteralNode>();
			EXPECT_EQ(as_lhs.type, PrimitiveType::Kind::INT32);
			EXPECT_EQ(as_lhs.value, Identifier::create("index"));

			ASSERT_TRUE(as_condition.rhs->Is<LiteralNode>());
			const auto& as_rhs = as_condition.rhs->As<LiteralNode>();
			EXPECT_EQ(as_rhs.type, PrimitiveType::Kind::INT32);
			EXPECT_EQ(as_rhs.value, Scalar::Create<PrimitiveType::Kind::INT32>(10));
		}

		ASSERT_TRUE(as_for_loop.update->Is<UnaryNode>());
		const auto& as_update = as_for_loop.update->As<UnaryNode>();
		EXPECT_EQ(as_update.type, PrimitiveType::Kind::INT32);
		EXPECT_EQ(as_update.op, ASTNode::Operator::INCREMENT);
		{
			ASSERT_TRUE(as_update.expression->Is<LiteralNode>());
			const auto& as_value = as_update.expression->As<LiteralNode>();
			EXPECT_EQ(as_value.type, PrimitiveType::Kind::INT32);
			EXPECT_EQ(as_value.value, Identifier::create("index"));
		}

		ASSERT_TRUE(as_for_loop.statements->Is<BlockNode>());
		const auto& as_statements = as_for_loop.statements->As<BlockNode>();
		ASSERT_EQ(as_statements.statements.size(), 1);
		EXPECT_EQ(as_statements.type, PrimitiveType::Kind::VOID);

		ASSERT_TRUE(as_statements.statements[0]->Is<LiteralNode>());
		const auto& as_statement = as_statements.statements[0]->As<LiteralNode>();
		EXPECT_EQ(as_statement.type, PrimitiveType::Kind::STRING);
		EXPECT_EQ(as_statement.value, Scalar::Create<PrimitiveType::Kind::STRING>("my_string"));
	}

	TEST_F(TypeResolverTest, ForLoop_ConditionNotBool)
	{
		auto condition = BinaryNode::Create(LiteralNode::Create(Identifier::create("index")),
		                                    LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(10)),
		                                    ASTNode::Operator::ADD);
		auto for_loop
			= ForLoopNode::Create(nullptr, std::move(condition), nullptr, BlockNode::Create(ASTNode::Dependencies{}));
		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(for_loop));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 1);

		ASSERT_TRUE(as_module.statements[0]->Is<ErrorNode>());
		const auto& as_error = as_module.statements[0]->As<ErrorNode>();
		EXPECT_EQ(as_error.message,
		          std::format("condition in for loop statement must be convertible to a '{}' type",
		                      std::string(Type{ PrimitiveType::Kind::BOOLEAN })));
	}

	TEST_F(TypeResolverTest, FunctionCallNode)
	{
		static constexpr auto k_function_name = "my_function";

		auto function_declaration_parameters = ASTNode::Dependencies{};
		function_declaration_parameters.emplace_back(
			VariableDeclarationNode::Create("a", k_base_specifier_str, nullptr, false));
		auto function_declaration = FunctionDeclarationNode::Create(k_function_name,
		                                                            k_base_specifier_f32,
		                                                            std::move(function_declaration_parameters),
		                                                            BlockNode::Create(ASTNode::Dependencies{}));

		auto function_call_parameters = ASTNode::Dependencies{};
		function_call_parameters.emplace_back(
			LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::STRING>("some_string")));
		auto function_call     = FunctionCallNode::Create(k_function_name, ASTNode::Dependencies{});
		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_call));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 1);

		ASSERT_TRUE(as_module.statements[0]->Is<ErrorNode>());
		const auto& as_error = as_module.statements[0]->As<ErrorNode>();
		EXPECT_EQ(as_error.message, std::format("cannot call non-existing function '{}'", k_function_name));
	}

	TEST_F(TypeResolverTest, FunctionCallNode_NonExistingFunction)
	{
		auto function_call     = FunctionCallNode::Create("non_existing", ASTNode::Dependencies{});
		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_call));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 1);

		ASSERT_TRUE(as_module.statements[0]->Is<ErrorNode>());
		const auto& as_error = as_module.statements[0]->As<ErrorNode>();
		EXPECT_EQ(as_error.message, "cannot call non-existing function 'non_existing'");
	}

	TEST_F(TypeResolverTest, FunctionCallNode_ParametersDoNotMatch)
	{
		static constexpr auto k_function_name = "my_function";

		auto function_declaration_parameters = ASTNode::Dependencies{};
		function_declaration_parameters.emplace_back(
			VariableDeclarationNode::Create("a", k_base_specifier_str, nullptr, false));
		auto function_declaration = FunctionDeclarationNode::Create(k_function_name,
		                                                            k_base_specifier_f32,
		                                                            std::move(function_declaration_parameters),
		                                                            BlockNode::Create(ASTNode::Dependencies{}));

		auto function_call_parameters = ASTNode::Dependencies{};
		function_call_parameters.emplace_back(
			LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::STRING>("some_string")));
		auto function_call     = FunctionCallNode::Create(k_function_name, ASTNode::Dependencies{});
		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(2);
		module_statements.push_back(std::move(function_declaration));
		module_statements.push_back(std::move(function_call));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 2);

		ASSERT_TRUE(as_module.statements[0]->Is<FunctionDeclarationNode>());
		const auto& as_function_declaration = as_module.statements[0]->As<FunctionDeclarationNode>();
		EXPECT_EQ(as_function_declaration.name, k_function_name);
		EXPECT_EQ(as_function_declaration.type_specifier, k_base_specifier_f32);
		EXPECT_EQ(as_function_declaration.parameters.size(), 1);
		EXPECT_TRUE(as_function_declaration.statements->As<BlockNode>().statements.empty());
		{
			ASSERT_TRUE(as_function_declaration.parameters[0]->Is<VariableDeclarationNode>());
			const auto& as_parameter = as_function_declaration.parameters[0]->As<VariableDeclarationNode>();
			EXPECT_EQ(as_parameter.name, "a");
			EXPECT_EQ(as_parameter.type_specifier, k_base_specifier_str);
			EXPECT_FALSE(as_parameter.expression);
			EXPECT_FALSE(as_parameter.is_mutable);
		}

		ASSERT_TRUE(as_module.statements[1]->Is<ErrorNode>());
		const auto& as_error = as_module.statements[1]->As<ErrorNode>();
		EXPECT_EQ(as_error.message, std::format("cannot call non-existing function '{}'", k_function_name));
	}

	TEST_F(TypeResolverTest, FunctionDeclarationNode)
	{
		auto function_declaration_parameters = ASTNode::Dependencies{};
		function_declaration_parameters.reserve(3);
		function_declaration_parameters.emplace_back(
			VariableDeclarationNode::Create("a", k_base_specifier_i32, nullptr, false));
		function_declaration_parameters.emplace_back(
			VariableDeclarationNode::Create("b", k_base_specifier_f64, nullptr, false));
		function_declaration_parameters.emplace_back(
			VariableDeclarationNode::Create("c", k_base_specifier_chr, nullptr, false));
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(VariableDeclarationNode::Create(
			"d", k_base_specifier_chr, LiteralNode::Create(Identifier::create("c")), false));
		auto function_declaration
			= FunctionDeclarationNode::Create("my_function",
		                                      k_base_specifier_str,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::Create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 1);

		ASSERT_TRUE(as_module.statements[0]->Is<FunctionDeclarationNode>());
		const auto& as_function_declaration = as_module.statements[0]->As<FunctionDeclarationNode>();
		EXPECT_EQ(as_function_declaration.name, "my_function");
		EXPECT_EQ(as_function_declaration.type_specifier, k_base_specifier_str);
		ASSERT_EQ(as_function_declaration.parameters.size(), 3);
		{
			ASSERT_TRUE(as_function_declaration.parameters[0]->Is<VariableDeclarationNode>());
			const auto& as_variable_declaration = as_function_declaration.parameters[0]->As<VariableDeclarationNode>();
			EXPECT_EQ(as_variable_declaration.name, "a");
			EXPECT_EQ(as_variable_declaration.type_specifier, k_base_specifier_i32);
			EXPECT_FALSE(as_variable_declaration.expression);
			EXPECT_FALSE(as_variable_declaration.is_mutable);
			EXPECT_EQ(as_variable_declaration.type, PrimitiveType::Kind::INT32);
		}
		{
			ASSERT_TRUE(as_function_declaration.parameters[1]->Is<VariableDeclarationNode>());
			const auto& as_variable_declaration = as_function_declaration.parameters[1]->As<VariableDeclarationNode>();
			EXPECT_EQ(as_variable_declaration.name, "b");
			EXPECT_EQ(as_variable_declaration.type_specifier, k_base_specifier_f64);
			EXPECT_FALSE(as_variable_declaration.expression);
			EXPECT_FALSE(as_variable_declaration.is_mutable);
			EXPECT_EQ(as_variable_declaration.type, PrimitiveType::Kind::FLOAT64);
		}
		{
			ASSERT_TRUE(as_function_declaration.parameters[2]->Is<VariableDeclarationNode>());
			const auto& as_variable_declaration = as_function_declaration.parameters[2]->As<VariableDeclarationNode>();
			EXPECT_EQ(as_variable_declaration.name, "c");
			EXPECT_EQ(as_variable_declaration.type_specifier, k_base_specifier_chr);
			EXPECT_FALSE(as_variable_declaration.expression);
			EXPECT_FALSE(as_variable_declaration.is_mutable);
			EXPECT_EQ(as_variable_declaration.type, PrimitiveType::Kind::CHAR);
		}
		ASSERT_TRUE(as_function_declaration.statements->Is<BlockNode>());
		const auto& as_statements = as_function_declaration.statements->As<BlockNode>();
		ASSERT_EQ(as_statements.statements.size(), 1);
		EXPECT_EQ(as_statements.type, PrimitiveType::Kind::VOID);

		ASSERT_TRUE(as_statements.statements[0]->Is<VariableDeclarationNode>());
		const auto& as_variable_declaration = as_statements.statements[0]->As<VariableDeclarationNode>();
		EXPECT_EQ(as_variable_declaration.name, "d");
		EXPECT_EQ(as_variable_declaration.type_specifier, k_base_specifier_chr);
		EXPECT_TRUE(as_variable_declaration.expression);
		{
			ASSERT_TRUE(as_variable_declaration.expression->Is<LiteralNode>());
			const auto& as_literal = as_variable_declaration.expression->As<LiteralNode>();
			EXPECT_EQ(as_literal.value, Identifier::create("c"));
			EXPECT_EQ(as_literal.type, PrimitiveType::Kind::CHAR);
		}
		EXPECT_FALSE(as_variable_declaration.is_mutable);
		EXPECT_EQ(as_variable_declaration.type, PrimitiveType::Kind::CHAR);
	}

	TEST_F(TypeResolverTest, FunctionDeclarationNode_ShadowsPreviousParameter)
	{
		auto function_declaration_parameters = ASTNode::Dependencies{};
		function_declaration_parameters.reserve(2);
		function_declaration_parameters.emplace_back(
			VariableDeclarationNode::Create("a", k_base_specifier_i32, nullptr, false));
		function_declaration_parameters.emplace_back(
			VariableDeclarationNode::Create("a", k_base_specifier_str, nullptr, false));
		auto function_declaration = FunctionDeclarationNode::Create("my_function",
		                                                            k_base_specifier_i32,
		                                                            std::move(function_declaration_parameters),
		                                                            BlockNode::Create(ASTNode::Dependencies{}));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 1);

		ASSERT_TRUE(as_module.statements[0]->Is<FunctionDeclarationNode>());
		const auto& as_function_declaration = as_module.statements[0]->As<FunctionDeclarationNode>();
		ASSERT_EQ(as_function_declaration.parameters.size(), 2);

		ASSERT_TRUE(as_function_declaration.parameters[0]->Is<VariableDeclarationNode>());
		const auto& as_parameter = as_function_declaration.parameters[0]->As<VariableDeclarationNode>();
		EXPECT_EQ(as_parameter.name, "a");
		EXPECT_EQ(as_parameter.type_specifier, k_base_specifier_i32);
		EXPECT_FALSE(as_parameter.expression);
		EXPECT_FALSE(as_parameter.is_mutable);

		ASSERT_TRUE(as_function_declaration.parameters[1]->Is<ErrorNode>());
		const auto& as_error = as_function_declaration.parameters[1]->As<ErrorNode>();
		EXPECT_EQ(as_error.message, "variable declaration 'a' shadows previous one");
	}

	TEST_F(TypeResolverTest, FunctionDeclarationNode_ShadowsPreviousOne)
	{
		static constexpr auto k_function_name = "my_function";

		auto first_function_declaration = FunctionDeclarationNode::Create(
			k_function_name, k_base_specifier_i32, ASTNode::Dependencies{}, BlockNode::Create(ASTNode::Dependencies{}));
		auto second_function_declaration = FunctionDeclarationNode::Create(
			k_function_name, k_base_specifier_i32, ASTNode::Dependencies{}, BlockNode::Create(ASTNode::Dependencies{}));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(2);
		module_statements.push_back(std::move(first_function_declaration));
		module_statements.push_back(std::move(second_function_declaration));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 2);

		ASSERT_TRUE(as_module.statements[0]->Is<FunctionDeclarationNode>());
		const auto& as_function_declaration = as_module.statements[0]->As<FunctionDeclarationNode>();
		EXPECT_EQ(as_function_declaration.name, k_function_name);
		EXPECT_EQ(as_function_declaration.type_specifier, k_base_specifier_i32);
		EXPECT_EQ(as_function_declaration.parameters.size(), 0);
		EXPECT_EQ(as_function_declaration.statements->As<BlockNode>().statements.size(), 0);
		EXPECT_EQ(as_function_declaration.type, PrimitiveType::Kind::INT32);

		ASSERT_TRUE(as_module.statements[1]->Is<ErrorNode>());
		const auto& as_error = as_module.statements[1]->As<ErrorNode>();
		EXPECT_EQ(as_error.message, std::format("function declaration '{}' shadows previous one", k_function_name));
	}

	TEST_F(TypeResolverTest, FunctionDeclarationNode_ShouldntShadowWithDifferentArguments)
	{
		static constexpr auto k_function_name = "my_function";

		auto first_function_declaration_parameters = ASTNode::Dependencies{};
		first_function_declaration_parameters.emplace_back(
			VariableDeclarationNode::Create("a", k_base_specifier_i32, nullptr, false));
		auto first_function_declaration
			= FunctionDeclarationNode::Create(k_function_name,
		                                      k_base_specifier_i32,
		                                      std::move(first_function_declaration_parameters),
		                                      BlockNode::Create(ASTNode::Dependencies{}));

		auto second_function_declaration_parameters = ASTNode::Dependencies{};
		second_function_declaration_parameters.emplace_back(
			VariableDeclarationNode::Create("a", k_base_specifier_f32, nullptr, false));
		auto second_function_declaration
			= FunctionDeclarationNode::Create(k_function_name,
		                                      k_base_specifier_i32,
		                                      std::move(second_function_declaration_parameters),
		                                      BlockNode::Create(ASTNode::Dependencies{}));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(2);
		module_statements.push_back(std::move(first_function_declaration));
		module_statements.push_back(std::move(second_function_declaration));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 2);

		{
			ASSERT_TRUE(as_module.statements[0]->Is<FunctionDeclarationNode>());
			const auto& as_function_declaration = as_module.statements[0]->As<FunctionDeclarationNode>();
			EXPECT_EQ(as_function_declaration.name, k_function_name);
			EXPECT_EQ(as_function_declaration.type_specifier, k_base_specifier_i32);
			EXPECT_EQ(as_function_declaration.parameters.size(), 1);
			EXPECT_EQ(as_function_declaration.statements->As<BlockNode>().statements.size(), 0);
			EXPECT_EQ(as_function_declaration.type, PrimitiveType::Kind::INT32);

			ASSERT_TRUE(as_function_declaration.parameters[0]->Is<VariableDeclarationNode>());
			const auto& as_parameter = as_function_declaration.parameters[0]->As<VariableDeclarationNode>();
			EXPECT_EQ(as_parameter.name, "a");
			EXPECT_EQ(as_parameter.type_specifier, k_base_specifier_i32);
			EXPECT_FALSE(as_parameter.expression);
			EXPECT_FALSE(as_parameter.is_mutable);
			EXPECT_EQ(as_parameter.type, PrimitiveType::Kind::INT32);
		}
		{
			ASSERT_TRUE(as_module.statements[1]->Is<FunctionDeclarationNode>());
			const auto& as_function_declaration = as_module.statements[1]->As<FunctionDeclarationNode>();
			EXPECT_EQ(as_function_declaration.name, k_function_name);
			EXPECT_EQ(as_function_declaration.type_specifier, k_base_specifier_i32);
			EXPECT_EQ(as_function_declaration.parameters.size(), 1);
			EXPECT_EQ(as_function_declaration.statements->As<BlockNode>().statements.size(), 0);
			EXPECT_EQ(as_function_declaration.type, PrimitiveType::Kind::INT32);

			ASSERT_TRUE(as_function_declaration.parameters[0]->Is<VariableDeclarationNode>());
			const auto& as_parameter = as_function_declaration.parameters[0]->As<VariableDeclarationNode>();
			EXPECT_EQ(as_parameter.name, "a");
			EXPECT_EQ(as_parameter.type_specifier, k_base_specifier_f32);
			EXPECT_FALSE(as_parameter.expression);
			EXPECT_FALSE(as_parameter.is_mutable);
			EXPECT_EQ(as_parameter.type, PrimitiveType::Kind::FLOAT32);
		}
	}

	TEST_F(TypeResolverTest, LiteralNode)
	{
		const auto get_value = [](PrimitiveType::Kind type) -> Value {
			switch (type) {
				case PrimitiveType::Kind::BOOLEAN:
					return Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true);
				case PrimitiveType::Kind::CHAR:
					return Scalar::Create<PrimitiveType::Kind::CHAR>('c');
				case PrimitiveType::Kind::FLOAT32:
					return Scalar::Create<PrimitiveType::Kind::FLOAT32>(1.0f);
				case PrimitiveType::Kind::FLOAT64:
					return Scalar::Create<PrimitiveType::Kind::FLOAT64>(10.0);
				case PrimitiveType::Kind::INT32:
					return Scalar::Create<PrimitiveType::Kind::INT32>(1);
				case PrimitiveType::Kind::INT64:
					return Scalar::Create<PrimitiveType::Kind::INT64>(10L);
				case PrimitiveType::Kind::STRING:
					return Scalar::Create<PrimitiveType::Kind::STRING>("my_string");
				case PrimitiveType::Kind::UNKNOWN:
				default:
					return Value{};
			}
		};

		static constexpr std::array k_literal_types = {
			PrimitiveType::Kind::BOOLEAN, PrimitiveType::Kind::CHAR,  PrimitiveType::Kind::FLOAT32,
			PrimitiveType::Kind::FLOAT64, PrimitiveType::Kind::INT32, PrimitiveType::Kind::INT64,
			PrimitiveType::Kind::STRING,
		};
		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(k_literal_types.size() + 2);
		module_statements.emplace_back(VariableDeclarationNode::Create("index", k_base_specifier_f32, nullptr, false));
		module_statements.emplace_back(LiteralNode::Create(Identifier::create("index")));
		for (const auto type : k_literal_types) {
			module_statements.emplace_back(LiteralNode::Create(get_value(type)));
		}
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), k_literal_types.size() + 2);

		ASSERT_TRUE(as_module.statements[0]->Is<VariableDeclarationNode>());
		const auto& as_variable_declaration = as_module.statements[0]->As<VariableDeclarationNode>();
		EXPECT_EQ(as_variable_declaration.name, "index");
		EXPECT_EQ(as_variable_declaration.type_specifier, k_base_specifier_f32);
		EXPECT_FALSE(as_variable_declaration.expression);
		EXPECT_FALSE(as_variable_declaration.is_mutable);
		EXPECT_EQ(as_variable_declaration.type, PrimitiveType::Kind::FLOAT32);

		// Identifier literal.
		ASSERT_TRUE(as_module.statements[1]->Is<LiteralNode>());
		const auto& as_identifier_literal = as_module.statements[1]->As<LiteralNode>();
		EXPECT_EQ(as_identifier_literal.type, PrimitiveType::Kind::FLOAT32);
		EXPECT_EQ(as_identifier_literal.value, Identifier::create("index"));

		for (std::size_t index = 0; index < k_literal_types.size(); ++index) {
			const auto literal_type = k_literal_types[index];
			ASSERT_TRUE(as_module.statements[index + 2]->Is<LiteralNode>());
			const auto& as_literal = as_module.statements[index + 2]->As<LiteralNode>();
			EXPECT_EQ(as_literal.type, literal_type);
			EXPECT_EQ(as_literal.value, get_value(literal_type));
		}
	}

	TEST_F(TypeResolverTest, LiteralNode_UndeclaredIdentifier)
	{
		static constexpr auto k_variable_name = "undeclared_variable";

		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(LiteralNode::Create(Identifier::create(k_variable_name)));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 1);

		ASSERT_TRUE(as_module.statements[0]->Is<ErrorNode>());
		const auto& as_error = as_module.statements[0]->As<ErrorNode>();
		EXPECT_EQ(as_error.message, std::format("use of undeclared identifier '{}'", k_variable_name));
	}

	TEST_F(TypeResolverTest, LoopControlNode)
	{
		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(2);
		module_statements.emplace_back(LoopControlNode::Create(LoopControlNode::Type::BREAK));
		module_statements.emplace_back(LoopControlNode::Create(LoopControlNode::Type::CONTINUE));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 2);

		ASSERT_TRUE(as_module.statements[0]->Is<LoopControlNode>());
		const auto& as_break = as_module.statements[0]->As<LoopControlNode>();
		EXPECT_EQ(as_break.control_type, LoopControlNode::Type::BREAK);
		EXPECT_EQ(as_break.type, PrimitiveType::Kind::VOID);

		ASSERT_TRUE(as_module.statements[1]->Is<LoopControlNode>());
		const auto& as_continue = as_module.statements[1]->As<LoopControlNode>();
		EXPECT_EQ(as_continue.control_type, LoopControlNode::Type::CONTINUE);
		EXPECT_EQ(as_continue.type, PrimitiveType::Kind::VOID);
	}

	TEST_F(TypeResolverTest, ReturnNode)
	{
		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(ReturnNode::Create());
		module_statements.emplace_back(
			ReturnNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::STRING>("my_string"))));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 2);

		{
			ASSERT_TRUE(as_module.statements[0]->Is<ReturnNode>());
			const auto& as_return = as_module.statements[0]->As<ReturnNode>();
			EXPECT_EQ(as_return.type, PrimitiveType::Kind::VOID);
			EXPECT_FALSE(as_return.expression);
		}

		{
			ASSERT_TRUE(as_module.statements[1]->Is<ReturnNode>());
			const auto& as_return = as_module.statements[1]->As<ReturnNode>();
			EXPECT_EQ(as_return.type, PrimitiveType::Kind::STRING);

			ASSERT_TRUE(as_return.expression);
			ASSERT_TRUE(as_return.expression->Is<LiteralNode>());
			const auto& as_literal = as_return.expression->As<LiteralNode>();
			EXPECT_EQ(as_literal.value, Scalar::Create<PrimitiveType::Kind::STRING>("my_string"));
			EXPECT_EQ(as_literal.type, PrimitiveType::Kind::STRING);
		}
	}

	TEST_F(TypeResolverTest, StructDeclarationNode)
	{
		auto struct_declaration_parameters = ASTNode::Dependencies{};
		struct_declaration_parameters.push_back(
			VariableDeclarationNode::Create("a", k_base_specifier_i32, nullptr, false));
		struct_declaration_parameters.push_back(
			VariableDeclarationNode::Create("b", k_base_specifier_f32, nullptr, false));
		struct_declaration_parameters.push_back(
			VariableDeclarationNode::Create("c", k_base_specifier_bool, nullptr, false));
		auto struct_declaration = StructDeclarationNode::Create("my_struct", std::move(struct_declaration_parameters));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(struct_declaration));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 1);

		const auto expected_type = Type{ StructType{ {
			PrimitiveType::Kind::INT32,
			PrimitiveType::Kind::FLOAT32,
			PrimitiveType::Kind::BOOLEAN,
		} } };

		ASSERT_TRUE(as_module.statements[0]->Is<StructDeclarationNode>());
		const auto& as_struct_declaration = as_module.statements[0]->As<StructDeclarationNode>();
		EXPECT_EQ(as_struct_declaration.name, "my_struct");
		EXPECT_EQ(as_struct_declaration.type, expected_type);
		ASSERT_EQ(as_struct_declaration.parameters.size(), 3);
		{
			ASSERT_TRUE(as_struct_declaration.parameters[0]->Is<VariableDeclarationNode>());
			const auto& as_parameter = as_struct_declaration.parameters[0]->As<VariableDeclarationNode>();
			EXPECT_EQ(as_parameter.name, "a");
			EXPECT_EQ(as_parameter.type_specifier, k_base_specifier_i32);
			EXPECT_FALSE(as_parameter.expression);
			EXPECT_FALSE(as_parameter.is_mutable);
		}
		{
			ASSERT_TRUE(as_struct_declaration.parameters[1]->Is<VariableDeclarationNode>());
			const auto& as_parameter = as_struct_declaration.parameters[1]->As<VariableDeclarationNode>();
			EXPECT_EQ(as_parameter.name, "b");
			EXPECT_EQ(as_parameter.type_specifier, k_base_specifier_f32);
			EXPECT_FALSE(as_parameter.expression);
			EXPECT_FALSE(as_parameter.is_mutable);
		}
		{
			ASSERT_TRUE(as_struct_declaration.parameters[2]->Is<VariableDeclarationNode>());
			const auto& as_parameter = as_struct_declaration.parameters[2]->As<VariableDeclarationNode>();
			EXPECT_EQ(as_parameter.name, "c");
			EXPECT_EQ(as_parameter.type_specifier, k_base_specifier_bool);
			EXPECT_FALSE(as_parameter.expression);
			EXPECT_FALSE(as_parameter.is_mutable);
		}
	}

	TEST_F(TypeResolverTest, UnaryNode_Arithmetic)
	{
		static constexpr std::array k_arithmetic_operators
			= { ASTNode::Operator::INCREMENT, ASTNode::Operator::DECREMENT };

		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(k_arithmetic_operators.size());
		for (const auto op : k_arithmetic_operators) {
			module_statements.emplace_back(
				UnaryNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT64>(1)), op));
		}
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), k_arithmetic_operators.size());

		for (std::size_t index = 0; index < k_arithmetic_operators.size(); ++index) {
			ASSERT_TRUE(as_module.statements[index]->Is<UnaryNode>());
			const auto& as_unary = as_module.statements[index]->As<UnaryNode>();
			EXPECT_EQ(as_unary.op, k_arithmetic_operators[index]);
			EXPECT_EQ(as_unary.type, PrimitiveType::Kind::INT64);

			ASSERT_TRUE(as_unary.expression->Is<LiteralNode>());
			const auto& as_expression = as_unary.expression->As<LiteralNode>();
			EXPECT_EQ(as_expression.value, Scalar::Create<PrimitiveType::Kind::INT64>(1));
			EXPECT_EQ(as_expression.type, PrimitiveType::Kind::INT64);
		}
	}

	TEST_F(TypeResolverTest, UnaryNode_Logical)
	{
		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(UnaryNode::Create(
			LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true)), ASTNode::Operator::LOGICAL_NOT));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 1);

		ASSERT_TRUE(as_module.statements[0]->Is<UnaryNode>());
		const auto& as_unary = as_module.statements[0]->As<UnaryNode>();
		EXPECT_EQ(as_unary.op, ASTNode::Operator::LOGICAL_NOT);
		EXPECT_EQ(as_unary.type, PrimitiveType::Kind::BOOLEAN);

		ASSERT_TRUE(as_unary.expression->Is<LiteralNode>());
		const auto& as_expression = as_unary.expression->As<LiteralNode>();
		EXPECT_EQ(as_expression.value, Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true));
		EXPECT_EQ(as_expression.type, PrimitiveType::Kind::BOOLEAN);
	}

	TEST_F(TypeResolverTest, UnaryNode_NoOverload)
	{
		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(
			UnaryNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::STRING>("my_string")),
		                      ASTNode::Operator::LOGICAL_NOT));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 1);

		ASSERT_TRUE(as_module.statements[0]->Is<ErrorNode>());
		const auto& as_error = as_module.statements[0]->As<ErrorNode>();
		EXPECT_EQ(as_error.message,
		          std::format("operator ('{}') does not exist for type '{}'",
		                      ASTNode::Name(ASTNode::Operator::LOGICAL_NOT),
		                      std::string(Type{ PrimitiveType::Kind::STRING })));
	}

	TEST_F(TypeResolverTest, VariableDeclaration_ShadowsPreviousOne)
	{
		static constexpr auto k_variable_name = "variable_to_be_shadowed";

		auto first_variable  = VariableDeclarationNode::Create(k_variable_name, k_base_specifier_f32, nullptr, true);
		auto second_variable = VariableDeclarationNode::Create(k_variable_name, k_base_specifier_i32, nullptr, false);

		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(2);
		module_statements.push_back(std::move(first_variable));
		module_statements.push_back(std::move(second_variable));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 2);

		ASSERT_TRUE(as_module.statements[0]->Is<VariableDeclarationNode>());
		const auto& as_variable_declaration = as_module.statements[0]->As<VariableDeclarationNode>();
		EXPECT_EQ(as_variable_declaration.name, k_variable_name);
		EXPECT_EQ(as_variable_declaration.type_specifier, k_base_specifier_f32);
		EXPECT_FALSE(as_variable_declaration.expression);
		EXPECT_TRUE(as_variable_declaration.is_mutable);
		EXPECT_EQ(as_variable_declaration.type, PrimitiveType::Kind::FLOAT32);

		ASSERT_TRUE(as_module.statements[1]->Is<ErrorNode>());
		const auto& as_error = as_module.statements[1]->As<ErrorNode>();
		EXPECT_EQ(as_error.message, std::format("variable declaration '{}' shadows previous one", k_variable_name));
	}

	TEST_F(TypeResolverTest, VariableDeclaration_ShadowsOuterScopeOne)
	{
		static constexpr auto k_variable_name = "variable_to_be_shadowed";

		auto first_variable = VariableDeclarationNode::Create(k_variable_name, k_base_specifier_f32, nullptr, true);

		auto inner_scope_statements = ASTNode::Dependencies{};
		inner_scope_statements.emplace_back(
			VariableDeclarationNode::Create(k_variable_name, k_base_specifier_i32, nullptr, false));
		auto inner_scope = BlockNode::Create(std::move(inner_scope_statements));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(2);
		module_statements.push_back(std::move(first_variable));
		module_statements.push_back(std::move(inner_scope));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 2);

		ASSERT_TRUE(as_module.statements[0]->Is<VariableDeclarationNode>());
		const auto& as_variable_declaration = as_module.statements[0]->As<VariableDeclarationNode>();
		EXPECT_EQ(as_variable_declaration.name, k_variable_name);
		EXPECT_EQ(as_variable_declaration.type_specifier, k_base_specifier_f32);
		EXPECT_FALSE(as_variable_declaration.expression);
		EXPECT_TRUE(as_variable_declaration.is_mutable);
		EXPECT_EQ(as_variable_declaration.type, PrimitiveType::Kind::FLOAT32);

		ASSERT_TRUE(as_module.statements[1]->Is<BlockNode>());
		const auto& as_inner_scope = as_module.statements[1]->As<BlockNode>();
		ASSERT_EQ(as_inner_scope.statements.size(), 1);
		EXPECT_EQ(as_inner_scope.type, PrimitiveType::Kind::VOID);

		ASSERT_TRUE(as_inner_scope.statements[0]->Is<ErrorNode>());
		const auto& as_error = as_inner_scope.statements[0]->As<ErrorNode>();
		EXPECT_EQ(as_error.message, std::format("variable declaration '{}' shadows previous one", k_variable_name));
	}

	TEST_F(TypeResolverTest, VariableDeclaration_ShouldntShadowPreviousInnerScope)
	{
		static constexpr auto k_variable_name = "variable_to_be_shadowed";

		auto inner_scope_statements = ASTNode::Dependencies{};
		inner_scope_statements.emplace_back(
			VariableDeclarationNode::Create(k_variable_name, k_base_specifier_i32, nullptr, false));
		auto inner_scope = BlockNode::Create(std::move(inner_scope_statements));

		auto outer_variable = VariableDeclarationNode::Create(k_variable_name, k_base_specifier_f32, nullptr, true);

		auto module_statements = ASTNode::Dependencies{};
		module_statements.reserve(2);
		module_statements.push_back(std::move(inner_scope));
		module_statements.push_back(std::move(outer_variable));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 2);

		ASSERT_TRUE(as_module.statements[0]->Is<BlockNode>());
		const auto& as_inner_scope = as_module.statements[0]->As<BlockNode>();
		ASSERT_EQ(as_inner_scope.statements.size(), 1);
		EXPECT_EQ(as_inner_scope.type, PrimitiveType::Kind::VOID);

		ASSERT_TRUE(as_inner_scope.statements[0]->Is<VariableDeclarationNode>());
		const auto& as_inner_variable = as_inner_scope.statements[0]->As<VariableDeclarationNode>();
		EXPECT_EQ(as_inner_variable.name, k_variable_name);
		EXPECT_EQ(as_inner_variable.type_specifier, k_base_specifier_i32);
		EXPECT_FALSE(as_inner_variable.expression);
		EXPECT_FALSE(as_inner_variable.is_mutable);
		EXPECT_EQ(as_inner_variable.type, PrimitiveType::Kind::INT32);

		ASSERT_TRUE(as_module.statements[1]->Is<VariableDeclarationNode>());
		const auto& as_outer_variable = as_module.statements[1]->As<VariableDeclarationNode>();
		EXPECT_EQ(as_outer_variable.name, k_variable_name);
		EXPECT_EQ(as_outer_variable.type_specifier, k_base_specifier_f32);
		EXPECT_FALSE(as_outer_variable.expression);
		EXPECT_TRUE(as_outer_variable.is_mutable);
		EXPECT_EQ(as_outer_variable.type, PrimitiveType::Kind::FLOAT32);
	}

	TEST_F(TypeResolverTest, While)
	{
		auto while_condition = LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true));

		auto while_statements = ASTNode::Dependencies{};
		while_statements.emplace_back(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::FLOAT64>(3.14)));

		auto while_loop = WhileNode::Create(std::move(while_condition), BlockNode::Create(std::move(while_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(while_loop));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());

		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_module.statements.size(), 1);

		ASSERT_TRUE(as_module.statements[0]->Is<WhileNode>());
		const auto& as_while = as_module.statements[0]->As<WhileNode>();

		ASSERT_TRUE(as_while.condition->Is<LiteralNode>());
		const auto& as_condition = as_while.condition->As<LiteralNode>();
		EXPECT_EQ(as_condition.type, PrimitiveType::Kind::BOOLEAN);

		ASSERT_TRUE(as_while.statements->Is<BlockNode>());
		const auto& as_statements = as_while.statements->As<BlockNode>();
		ASSERT_EQ(as_statements.statements.size(), 1);
		EXPECT_EQ(as_statements.type, PrimitiveType::Kind::VOID);

		ASSERT_TRUE(as_statements.statements[0]->Is<LiteralNode>());
		const auto& as_statement = as_statements.statements[0]->As<LiteralNode>();
		EXPECT_EQ(as_statement.value, Scalar::Create<PrimitiveType::Kind::FLOAT64>(3.14));
		EXPECT_EQ(as_statement.type, PrimitiveType::Kind::FLOAT64);
	}

	TEST_F(TypeResolverTest, PointerToPrimtiveType)
	{
		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(VariableDeclarationNode::Create(
			"my_variable", TypeSpecifier(PointerTypeSpecifier(k_base_specifier_i32)), LiteralNode::Create({}), false));
		auto expected_module = ModuleNode::Create(k_module_name, std::move(module_statements));

		auto result_module = resolve(expected_module.get());
		EXPECT_NE(expected_module.get(), result_module.get());
		ASSERT_TRUE(result_module->Is<ModuleNode>());

		// TODO: Resolve pointer types.
	}

}  // namespace Soul::AST::Visitors::UT
