#include "ast/visitors/lower.h"

#include <gtest/gtest.h>

#include "ast/ast.h"
#include "ast/visitors/desugar.h"
#include "ast/visitors/error_collector.h"
#include "ast/visitors/type_discoverer.h"
#include "ast/visitors/type_resolver.h"
#include "common/compile_time_dictionary.h"
#include "ir/instruction.h"
#include "ir/ir.h"
#include "ir/visitors/print.h"

namespace soul::ast::visitors::ut
{
	using namespace soul::types;
	using namespace soul::ir;
	using namespace soul::ir::visitors;

	class LowerVisitorTest : public ::testing::Test
	{
		public:
		static constexpr auto k_module_name   = "codegen_module";
		static constexpr auto k_function_name = "codegen_function";

		protected:
		// NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
		std::pair<std::string, std::string> compare(const Module& expected, const Module& result)
		{
			ir::visitors::PrintVisitor print_result{};
			print_result.accept(result);

			ir::visitors::PrintVisitor print_expected{};
			print_expected.accept(expected);

			return std::make_pair(std::move(print_expected.string()), std::move(print_result.string()));
		}

		std::unique_ptr<Module> build(ASTNode::Dependency&& root)
		{
			const auto verify = [](ASTNode::Dependency&& root) -> ASTNode::Dependency {
				ErrorCollectorVisitor error_collector{};
				error_collector.accept(root.get());
				if (!error_collector.is_valid()) {
					for (const auto& [depth, error] : error_collector.errors()) {
						std::cerr << std::format("[{}]: {}\n", depth, error->message);
					}
					return nullptr;
				}
				return root;
			};

			TypeDiscovererVisitor type_discoverer_visitor{};
			type_discoverer_visitor.accept(root.get());
			auto type_discoverer_root = verify(type_discoverer_visitor.cloned());
			if (!type_discoverer_root) {
				return nullptr;
			}

			TypeResolverVisitor type_resolver_visitor{ type_discoverer_visitor.discovered_types() };
			type_resolver_visitor.accept(type_discoverer_root.get());
			auto type_resolver_root = verify(type_resolver_visitor.cloned());
			if (!type_resolver_root) {
				return nullptr;
			}

			DesugarVisitor desugar_visitor{};
			desugar_visitor.accept(type_resolver_root.get());
			auto desugar_visitor_root = verify(desugar_visitor.cloned());
			if (!desugar_visitor_root) {
				return nullptr;
			}

			LowerVisitor lower_visitor{};
			lower_visitor.accept(desugar_visitor_root.get());
			return lower_visitor.get();
		}
	};

	TEST_F(LowerVisitorTest, Block)
	{
		auto inner_scope_statements = ASTNode::Dependencies{};
		inner_scope_statements.reserve(3);
		inner_scope_statements.emplace_back(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Boolean>(true), LiteralNode::Type::Boolean));
		inner_scope_statements.emplace_back(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::String>("my_string"), LiteralNode::Type::String));
		inner_scope_statements.emplace_back(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(123), LiteralNode::Type::Int64));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(BlockNode::create(std::move(inner_scope_statements)));
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      "i32",
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, Type{ PrimitiveType::Kind::Int32 }, {});
		auto* inner_block = expected_ir_builder.create_basic_block();
		expected_ir_builder.connect(expected_ir_builder.current_basic_block(), inner_block);
		expected_ir_builder.emit<Jump>(inner_block);
		expected_ir_builder.switch_to(inner_block);
		expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::Boolean },
		                                Scalar::create<PrimitiveType::Kind::Boolean>(true));
		expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::String },
		                                Scalar::create<PrimitiveType::Kind::String>("my_string"));
		expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::Int64 },
		                                Scalar::create<PrimitiveType::Kind::Int32>(123));
		const auto& expected_ir = expected_ir_builder.build();

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, Cast)
	{
		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(CastNode::create(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(123), LiteralNode::Type::Int32), "str"));
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      "i32",
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, Type{ PrimitiveType::Kind::Int32 }, {});
		auto* instr = expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::Int32 },
		                                              Scalar::create<PrimitiveType::Kind::Int32>(123));
		expected_ir_builder.emit<Cast>(Type{ PrimitiveType::Kind::String }, instr);
		const auto& expected_ir = expected_ir_builder.build();

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, FunctionCall)
	{
		static constexpr auto k_function_to_call = "function_to_be_called";

		auto first_function_declaration_parameters = ASTNode::Dependencies{};
		first_function_declaration_parameters.reserve(2);
		first_function_declaration_parameters.emplace_back(VariableDeclarationNode::create("a", "str", nullptr, true));
		first_function_declaration_parameters.emplace_back(VariableDeclarationNode::create("b", "bool", nullptr, true));
		auto first_function_declaration_statements = ASTNode::Dependencies{};
		auto first_function_declaration
			= FunctionDeclarationNode::create(k_function_to_call,
		                                      "i32",
		                                      std::move(first_function_declaration_parameters),
		                                      BlockNode::create(std::move(first_function_declaration_statements)));

		auto second_function_declaration_parameters = ASTNode::Dependencies{};
		auto second_function_declaration_statements = ASTNode::Dependencies{};
		auto first_function_call_parameters         = ASTNode::Dependencies{};
		first_function_call_parameters.reserve(2);
		first_function_call_parameters.emplace_back(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::String>("my_string"), LiteralNode::Type::String));
		first_function_call_parameters.emplace_back(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Boolean>(true), LiteralNode::Type::Boolean));
		// @TODO
		// first_function_call_parameters.emplace_back(ReturnNode::create(LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(1,
		//       LiteralNode::Type::Kind::Int32 ));

		second_function_declaration_statements.reserve(2);
		second_function_declaration_statements.emplace_back(VariableDeclarationNode::create(
			"variable",
			"i32",
			FunctionCallNode::create(k_function_to_call, std::move(first_function_call_parameters)),
			false));
		auto second_function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      "void",
		                                      std::move(second_function_declaration_parameters),
		                                      BlockNode::create(std::move(second_function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(first_function_declaration));
		module_statements.push_back(std::move(second_function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(
			k_function_to_call,
			Type{ PrimitiveType::Kind::Int32 },
			{ Type{ PrimitiveType::Kind::String }, Type{ PrimitiveType::Kind::Boolean } });
		expected_ir_builder.emit_upsilon("a", nullptr);  // @TODO GetArgument(0) instruction
		expected_ir_builder.emit_upsilon("b", nullptr);  // @TODO GetArgument(1) instruction
		expected_ir_builder.create_function(k_function_name, Type{ PrimitiveType::Kind::Void }, {});
		auto* v1   = expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::String },
                                                   Scalar::create<PrimitiveType::Kind::String>("my_string"));
		auto* v2   = expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::Boolean },
                                                   Scalar::create<PrimitiveType::Kind::Boolean>(true));
		auto* call = expected_ir_builder.emit<Call>(
			Type{ PrimitiveType::Kind::Int32 }, k_function_to_call, std::vector<Instruction*>{ v1, v2 });
		expected_ir_builder.emit_upsilon("variable", call);

		const auto& expected_ir = expected_ir_builder.build();

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, If)
	{
		auto if_node_condition
			= LiteralNode::create(Scalar::create<PrimitiveType::Kind::Boolean>(true), LiteralNode::Type::Boolean);
		auto if_then_statements = ASTNode::Dependencies{};
		if_then_statements.emplace_back(LiteralNode::create(
			Scalar::create<PrimitiveType::Kind::String>("then_branch_string"), LiteralNode::Type::String));
		auto if_else_statements = ASTNode::Dependencies{};
		if_else_statements.emplace_back(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Boolean>(false), LiteralNode::Type::Boolean));

		auto if_node = IfNode::create(std::move(if_node_condition),
		                              BlockNode::create(std::move(if_then_statements)),
		                              BlockNode::create(std::move(if_else_statements)));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.push_back(std::move(if_node));
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      "i32",
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, Type{ PrimitiveType::Kind::Int32 }, {});
		auto* input_block  = expected_ir_builder.current_basic_block();
		auto* then_block   = expected_ir_builder.create_basic_block();
		auto* else_block   = expected_ir_builder.create_basic_block();
		auto* output_block = expected_ir_builder.create_basic_block();
		expected_ir_builder.connect(input_block, std::array{ then_block, else_block });
		expected_ir_builder.connect(std::array{ then_block, else_block }, output_block);

		auto* condition = expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::Boolean },
		                                                  Scalar::create<PrimitiveType::Kind::Boolean>(true));
		expected_ir_builder.emit<JumpIf>(condition, then_block, else_block);

		expected_ir_builder.switch_to(then_block);
		expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::String },
		                                Scalar::create<PrimitiveType::Kind::String>("then_branch_string"));
		expected_ir_builder.emit<Jump>(output_block);

		expected_ir_builder.switch_to(else_block);
		expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::Boolean },
		                                Scalar::create<PrimitiveType::Kind::Boolean>(false));
		expected_ir_builder.emit<Jump>(output_block);
		const auto& expected_ir = expected_ir_builder.build();

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, Literals)
	{
		static const std::array k_input_values = {
			std::make_tuple(Type{ PrimitiveType::Kind::Boolean },
			                LiteralNode::Type::Boolean,
			                Value{ Scalar::create<PrimitiveType::Kind::Boolean>(true) }),
			std::make_tuple(Type{ PrimitiveType::Kind::Char },
			                LiteralNode::Type::Char,
			                Value{ Scalar::create<PrimitiveType::Kind::Char>('c') }),
			std::make_tuple(Type{ PrimitiveType::Kind::Float32 },
			                LiteralNode::Type::Float32,
			                Value{ Scalar::create<PrimitiveType::Kind::Float32>(3.14f) }),
			std::make_tuple(Type{ PrimitiveType::Kind::Float64 },
			                LiteralNode::Type::Float64,
			                Value{ Scalar::create<PrimitiveType::Kind::Float64>(5.46) }),
			std::make_tuple(Type{ PrimitiveType::Kind::Int32 },
			                LiteralNode::Type::Int32,
			                Value{ Scalar::create<PrimitiveType::Kind::Int32>(123) }),
			std::make_tuple(Type{ PrimitiveType::Kind::Int64 },
			                LiteralNode::Type::Int64,
			                Value{ Scalar::create<PrimitiveType::Kind::Int64>(456L) }),
			std::make_tuple(Type{ PrimitiveType::Kind::String },
			                LiteralNode::Type::String,
			                Value{ Scalar::create<PrimitiveType::Kind::String>("my_string") }),
		};

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.reserve(k_input_values.size());
		for (const auto& [_, literal_type, value] : k_input_values) {
			function_declaration_statements.emplace_back(LiteralNode::create(value, literal_type));
		}
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      "i32",
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, Type{ PrimitiveType::Kind::Int32 }, {});
		for (const auto& [type, _, value] : k_input_values) {
			expected_ir_builder.emit<Const>(type, value);
		}
		const auto& expected_ir = expected_ir_builder.build();

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, While)
	{
		static constexpr auto k_variable_name = "index";

		auto index_node = VariableDeclarationNode::create(
			k_variable_name,
			"i32",
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(0), LiteralNode::Type::Int32),
			true);

		auto while_node_condition = BinaryNode::create(
			LiteralNode::create(Value{ Identifier::create(k_variable_name) }, LiteralNode::Type::Identifier),
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(10), LiteralNode::Type::Int32),
			ASTNode::Operator::Less);
		auto while_node_statements = ASTNode::Dependencies{};
		while_node_statements.emplace_back(BinaryNode::create(
			LiteralNode::create(Value{ Identifier::create(k_variable_name) }, LiteralNode::Type::Identifier),
			BinaryNode::create(
				LiteralNode::create(Value{ Identifier::create(k_variable_name) }, LiteralNode::Type::Identifier),
				LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(1), LiteralNode::Type::Int32),
				ASTNode::Operator::Add),
			ASTNode::Operator::Assign));

		auto while_node
			= WhileNode::create(std::move(while_node_condition), BlockNode::create(std::move(while_node_statements)));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.push_back(std::move(index_node));
		function_declaration_statements.push_back(std::move(while_node));
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      "i32",
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, Type{ PrimitiveType::Kind::Int32 }, {});
		auto* input_block     = expected_ir_builder.current_basic_block();
		auto* condition_block = expected_ir_builder.create_basic_block();
		auto* body_block      = expected_ir_builder.create_basic_block();
		auto* output_block    = expected_ir_builder.create_basic_block();

		expected_ir_builder.connect(input_block, condition_block);
		expected_ir_builder.connect(condition_block, std::array{ body_block, output_block });
		expected_ir_builder.connect(body_block, condition_block);

		expected_ir_builder.switch_to(input_block);
		{
			auto* initial_value = expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::Int32 },
			                                                      Scalar::create<PrimitiveType::Kind::Int32>(0));
			expected_ir_builder.emit_upsilon(k_variable_name, initial_value);
			expected_ir_builder.emit<Jump>(condition_block);
		}

		expected_ir_builder.switch_to(condition_block);
		{
			auto* lhs       = expected_ir_builder.emit_phi(k_variable_name, Type{ PrimitiveType::Kind::Int32 });
			auto* rhs       = expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::Int32 },
                                                        Scalar::create<PrimitiveType::Kind::Int32>(10));
			auto* condition = expected_ir_builder.emit<Less>(lhs, rhs);
			expected_ir_builder.emit<JumpIf>(condition, body_block, output_block);
		}

		expected_ir_builder.switch_to(body_block);
		{
			auto* lhs        = expected_ir_builder.emit_phi(k_variable_name, PrimitiveType::Kind::Int32);
			auto* rhs        = expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::Int32 },
                                                        Scalar::create<PrimitiveType::Kind::Int32>(1));
			auto* expression = expected_ir_builder.emit<Add>(Type{ PrimitiveType::Kind::Int32 }, lhs, rhs);
			expected_ir_builder.emit_upsilon(k_variable_name, expression);
			expected_ir_builder.emit<Jump>(condition_block);
		}
		const auto& expected_ir = expected_ir_builder.build();

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, Module_Empty)
	{
		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		auto expected_ir = expected_ir_builder.build();
		auto result_ir   = build(ModuleNode::create(k_module_name, ASTNode::Dependencies{}));
		ASSERT_TRUE(result_ir);

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, Unary)
	{
		// NOTE: It doesn't matter that the actual value has invalid type.
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(UnaryNode::create(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Boolean>(true), LiteralNode::Type::Boolean),
			ASTNode::Operator::LogicalNot));
		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration
			= FunctionDeclarationNode::create(this->k_function_name,
		                                      "void",
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = this->build(ModuleNode::create(this->k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(this->k_module_name);
		expected_ir_builder.create_function(this->k_function_name, Type{ PrimitiveType::Kind::Void }, {});
		auto* expression = expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::Boolean },
		                                                   Scalar::create<PrimitiveType::Kind::Boolean>(true));
		expected_ir_builder.emit<Not>(expression);
		auto expected_ir = expected_ir_builder.build();
		ASSERT_TRUE(expected_ir);

		auto [expected_string, result_string] = this->compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, VariableDeclaration_SimpleDeclarationAndAssignment)
	{
		static constexpr auto k_first_variable_name  = "first_variable";
		static constexpr auto k_second_variable_name = "second_variable";

		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.reserve(4);
		function_declaration_statements.emplace_back(VariableDeclarationNode::create(
			k_first_variable_name,
			"i32",
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(1), LiteralNode::Type::Int32),
			true));
		function_declaration_statements.emplace_back(BinaryNode::create(
			LiteralNode::create(Identifier::create(k_first_variable_name), LiteralNode::Type::Identifier),
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(3), LiteralNode::Type::Int32),
			ASTNode::Operator::Assign));
		function_declaration_statements.emplace_back(VariableDeclarationNode::create(
			k_second_variable_name,
			"i32",
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(5), LiteralNode::Type::Int32),
			false));
		function_declaration_statements.emplace_back(BinaryNode::create(
			LiteralNode::create(Identifier::create(k_second_variable_name), LiteralNode::Type::Identifier),
			LiteralNode::create(Identifier::create(k_first_variable_name), LiteralNode::Type::Identifier),
			ASTNode::Operator::Assign));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      "void",
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, Type{ PrimitiveType::Kind::Void }, {});
		auto* first_value = expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::Int32 },
		                                                    Scalar::create<PrimitiveType::Kind::Int32>(1));
		expected_ir_builder.emit_upsilon(k_first_variable_name, first_value);
		auto* second_value = expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::Int32 },
		                                                     Scalar::create<PrimitiveType::Kind::Int32>(3));
		expected_ir_builder.emit_upsilon(k_first_variable_name, second_value);
		auto* third_value = expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::Int32 },
		                                                    Scalar::create<PrimitiveType::Kind::Int32>(5));
		expected_ir_builder.emit_upsilon(k_second_variable_name, third_value);
		auto* first_phi = expected_ir_builder.emit_phi(k_first_variable_name, Type{ PrimitiveType::Kind::Int32 });
		expected_ir_builder.emit_upsilon(k_second_variable_name, first_phi);

		auto expected_ir = expected_ir_builder.build();
		ASSERT_TRUE(expected_ir);

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, VariableDeclaration_ReadTheValueTwice)
	{
		static constexpr auto k_first_variable_name  = "first_variable";
		static constexpr auto k_second_variable_name = "second_variable";

		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.reserve(2);
		function_declaration_statements.emplace_back(VariableDeclarationNode::create(
			k_first_variable_name,
			"i32",
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(1), LiteralNode::Type::Int32),
			false));
		function_declaration_statements.emplace_back(VariableDeclarationNode::create(
			k_second_variable_name,
			"i32",
			BinaryNode::create(
				LiteralNode::create(Identifier::create(k_first_variable_name), LiteralNode::Type::Identifier),
				LiteralNode::create(Identifier::create(k_first_variable_name), LiteralNode::Type::Identifier),
				ASTNode::Operator::Mul),
			false));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      "void",
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, Type{ PrimitiveType::Kind::Void }, {});
		auto* value = expected_ir_builder.emit<Const>(Type{ PrimitiveType::Kind::Int32 },
		                                              Scalar::create<PrimitiveType::Kind::Int32>(1));
		expected_ir_builder.emit_upsilon(k_first_variable_name, value);
		auto* lhs    = expected_ir_builder.emit_phi(k_first_variable_name, Type{ PrimitiveType::Kind::Int32 });
		auto* rhs    = expected_ir_builder.emit_phi(k_first_variable_name, Type{ PrimitiveType::Kind::Int32 });
		auto* result = expected_ir_builder.emit<Mul>(Type{ PrimitiveType::Kind::Int32 }, lhs, rhs);
		expected_ir_builder.emit_upsilon(k_second_variable_name, result);
		auto expected_ir = expected_ir_builder.build();
		ASSERT_TRUE(expected_ir);

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	template <typename T>
	class LowerVisitorBinaryTypedTest : public LowerVisitorTest
	{
	};

	template <ASTNode::Operator Op, PrimitiveType::Kind Type>
	struct BinaryCase
	{
		static constexpr auto k_operator = Op;
		static constexpr auto k_type     = Type;
	};

	template <PrimitiveType::Kind V>
	static constexpr auto k_primitive_to_literal_type
		= VVDictionary<V,
	                   VVTranslation<PrimitiveType::Kind::Boolean, LiteralNode::Type::Boolean>,
	                   VVTranslation<PrimitiveType::Kind::Char, LiteralNode::Type::Char>,
	                   VVTranslation<PrimitiveType::Kind::Float32, LiteralNode::Type::Float32>,
	                   VVTranslation<PrimitiveType::Kind::Float64, LiteralNode::Type::Float64>,
	                   VVTranslation<PrimitiveType::Kind::Int32, LiteralNode::Type::Int32>,
	                   VVTranslation<PrimitiveType::Kind::Int64, LiteralNode::Type::Int64>,
	                   VVTranslation<PrimitiveType::Kind::String, LiteralNode::Type::String>>::k_value;

	template <ASTNode::Operator Op>
	using BinaryInstruction = VTDictionary<Op,
	                                       VTTranslation<ASTNode::Operator::Add, ir::Add>,
	                                       VTTranslation<ASTNode::Operator::Sub, ir::Sub>,
	                                       VTTranslation<ASTNode::Operator::Mul, ir::Mul>,
	                                       VTTranslation<ASTNode::Operator::Div, ir::Div>,
	                                       VTTranslation<ASTNode::Operator::Mod, ir::Mod>,
	                                       VTTranslation<ASTNode::Operator::Equal, ir::Equal>,
	                                       VTTranslation<ASTNode::Operator::NotEqual, ir::NotEqual>,
	                                       VTTranslation<ASTNode::Operator::Greater, ir::Greater>,
	                                       VTTranslation<ASTNode::Operator::GreaterEqual, ir::GreaterEqual>,
	                                       VTTranslation<ASTNode::Operator::Less, ir::Less>,
	                                       VTTranslation<ASTNode::Operator::LessEqual, ir::LessEqual>,
	                                       VTTranslation<ASTNode::Operator::LogicalAnd, ir::And>,
	                                       VTTranslation<ASTNode::Operator::LogicalOr, ir::Or>>::Type;

	using BinaryTypes = ::testing::Types<BinaryCase<ASTNode::Operator::Add, PrimitiveType::Kind::Int32>,
	                                     BinaryCase<ASTNode::Operator::Sub, PrimitiveType::Kind::Int32>,
	                                     BinaryCase<ASTNode::Operator::Mul, PrimitiveType::Kind::Int32>,
	                                     BinaryCase<ASTNode::Operator::Div, PrimitiveType::Kind::Int32>,
	                                     BinaryCase<ASTNode::Operator::Mod, PrimitiveType::Kind::Int32>,
	                                     BinaryCase<ASTNode::Operator::Equal, PrimitiveType::Kind::Float32>,
	                                     BinaryCase<ASTNode::Operator::NotEqual, PrimitiveType::Kind::Float32>,
	                                     BinaryCase<ASTNode::Operator::Greater, PrimitiveType::Kind::Float32>,
	                                     BinaryCase<ASTNode::Operator::GreaterEqual, PrimitiveType::Kind::Float32>,
	                                     BinaryCase<ASTNode::Operator::Less, PrimitiveType::Kind::Float32>,
	                                     BinaryCase<ASTNode::Operator::LessEqual, PrimitiveType::Kind::Float32>,
	                                     BinaryCase<ASTNode::Operator::LogicalAnd, PrimitiveType::Kind::Boolean>,
	                                     BinaryCase<ASTNode::Operator::LogicalOr, PrimitiveType::Kind::Boolean>>;
	TYPED_TEST_SUITE(LowerVisitorBinaryTypedTest, BinaryTypes);

	TYPED_TEST(LowerVisitorBinaryTypedTest, Binary)
	{
		using InstructionCase                = BinaryInstruction<TypeParam::k_operator>;
		static constexpr auto k_type         = TypeParam::k_type;
		static constexpr auto k_literal_type = k_primitive_to_literal_type<k_type>;

		// NOTE: It doesn't matter that the actual value has invalid type.
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(
			BinaryNode::create(LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(1), k_literal_type),
		                       LiteralNode::create(Scalar::create<PrimitiveType::Kind::Int32>(2), k_literal_type),
		                       TypeParam::k_operator));
		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration
			= FunctionDeclarationNode::create(this->k_function_name,
		                                      "void",
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = this->build(ModuleNode::create(this->k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(this->k_module_name);
		expected_ir_builder.create_function(this->k_function_name, Type{ PrimitiveType::Kind::Void }, {});
		auto* v1 = expected_ir_builder.emit<Const>(Type{ k_type }, Scalar::create<PrimitiveType::Kind::Int32>(1));
		auto* v2 = expected_ir_builder.emit<Const>(Type{ k_type }, Scalar::create<PrimitiveType::Kind::Int32>(2));
		if constexpr (std::is_constructible_v<InstructionCase, types::Type, Instruction*, Instruction*>) {
			expected_ir_builder.emit<InstructionCase>(Type{ k_type }, v1, v2);
		} else if constexpr (std::is_constructible_v<InstructionCase, Instruction*, Instruction*>) {
			expected_ir_builder.emit<InstructionCase>(v1, v2);
		} else {
			GTEST_FAIL() << "unhandled instruction case";
		}
		auto expected_ir = expected_ir_builder.build();
		ASSERT_TRUE(expected_ir);

		auto [expected_string, result_string] = this->compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}
}  // namespace soul::ast::visitors::ut
