#include "AST/Visitors/Lower.h"

#include <gtest/gtest.h>

#include "AST/AST.h"
#include "AST/Visitors/Desugar.h"
#include "AST/Visitors/ErrorCollector.h"
#include "AST/Visitors/TypeDiscoverer.h"
#include "AST/Visitors/TypeResolver.h"
#include "Common/CompileTimeDictionary.h"
#include "IR/IR.h"
#include "IR/Instruction.h"
#include "IR/Visitors/Print.h"

namespace Soul::AST::Visitors::UT
{
	using namespace Soul::Types;
	using namespace Soul::Parser;
	using namespace Soul::IR;
	using namespace Soul::IR::Visitors;

	class LowerVisitorTest : public ::testing::Test
	{
		public:
		static constexpr auto k_module_name   = "codegen_module";
		static constexpr auto k_function_name = "codegen_function";

		protected:
		static std::pair<std::string, std::string> Compare(const Module& expected, const Module& result)
		{
			PrintVisitor print_result{};
			print_result.Accept(result);

			PrintVisitor print_expected{};
			print_expected.Accept(expected);

			return std::make_pair(std::move(print_expected.String()), std::move(print_result.String()));
		}

		static std::unique_ptr<Module> Build(ASTNode::Dependency&& root)
		{
			const auto verify = [](ASTNode::Dependency&& root_node) -> ASTNode::Dependency {
				ErrorCollectorVisitor error_collector{};
				error_collector.Accept(root_node.get());
				if (!error_collector.IsValid()) {
					for (const auto& [depth, error] : error_collector.GetErrors()) {
						std::cerr << std::format("[{}]: {}\n", depth, error->message);
					}
					return nullptr;
				}
				return root_node;
			};

			TypeDiscovererVisitor type_discoverer_visitor{};
			type_discoverer_visitor.Accept(root.get());
			auto type_discoverer_root = verify(type_discoverer_visitor.Cloned());
			if (!type_discoverer_root) {
				return nullptr;
			}

			TypeResolverVisitor type_resolver_visitor{ type_discoverer_visitor.GetDiscoveredTypes() };
			type_resolver_visitor.Accept(type_discoverer_root.get());
			auto type_resolver_root = verify(type_resolver_visitor.Cloned());
			if (!type_resolver_root) {
				return nullptr;
			}

			DesugarVisitor desugar_visitor{};
			desugar_visitor.Accept(type_resolver_root.get());
			auto desugar_visitor_root = verify(desugar_visitor.Cloned());
			if (!desugar_visitor_root) {
				return nullptr;
			}

			LowerVisitor lower_visitor{};
			lower_visitor.Accept(desugar_visitor_root.get());
			return lower_visitor.Get();
		}

		template <PrimitiveType::Kind T>
		static Instruction* EmitConst(IRBuilder& builder, Detail::PrimitiveKindToValueType<T>&& value)
		{
			return builder.Emit<Const>(T, Scalar::Create<T>(std::move(value)));
		}
	};

	TEST_F(LowerVisitorTest, Block)
	{
		auto inner_scope_statements = ASTNode::Dependencies{};
		inner_scope_statements.reserve(3);
		inner_scope_statements.emplace_back(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true)));
		inner_scope_statements.emplace_back(
			LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::STRING>("my_string")));
		inner_scope_statements.emplace_back(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT64>(123)));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(BlockNode::Create(std::move(inner_scope_statements)));
		auto function_declaration
			= FunctionDeclarationNode::Create(k_function_name,
		                                      k_base_specifier_i32,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::Create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = Build(ModuleNode::Create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.SetModuleName(k_module_name);
		expected_ir_builder.CreateFunction(k_function_name, PrimitiveType::Kind::INT32, {});
		auto* inner_block = expected_ir_builder.CreateBasicBlock();
		expected_ir_builder.Emit<Jump>(inner_block);
		expected_ir_builder.SwitchTo(inner_block);
		EmitConst<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, true);
		EmitConst<PrimitiveType::Kind::STRING>(expected_ir_builder, "my_string");
		EmitConst<PrimitiveType::Kind::INT64>(expected_ir_builder, 123);
		const auto& expected_ir = expected_ir_builder.Build();

		auto [expected_string, result_string] = Compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, Cast)
	{
		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(CastNode::Create(
			LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(123)), k_base_specifier_str));
		auto function_declaration
			= FunctionDeclarationNode::Create(k_function_name,
		                                      k_base_specifier_i32,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::Create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = Build(ModuleNode::Create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.SetModuleName(k_module_name);
		expected_ir_builder.CreateFunction(k_function_name, PrimitiveType::Kind::INT32, {});
		auto* instr = EmitConst<PrimitiveType::Kind::INT32>(expected_ir_builder, 123);
		expected_ir_builder.Emit<Cast>(PrimitiveType::Kind::STRING, instr);
		const auto& expected_ir = expected_ir_builder.Build();

		auto [expected_string, result_string] = Compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, FunctionCall)
	{
		static constexpr auto k_function_to_call = "function_to_be_called";

		auto first_function_declaration_parameters = ASTNode::Dependencies{};
		first_function_declaration_parameters.reserve(2);
		first_function_declaration_parameters.emplace_back(
			VariableDeclarationNode::Create("a", k_base_specifier_str, nullptr, true));
		first_function_declaration_parameters.emplace_back(
			VariableDeclarationNode::Create("b", k_base_specifier_bool, nullptr, true));
		auto first_function_declaration_statements = ASTNode::Dependencies{};
		first_function_declaration_statements.emplace_back(
			ReturnNode::Create(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(1))));
		auto first_function_declaration
			= FunctionDeclarationNode::Create(k_function_to_call,
		                                      k_base_specifier_i32,
		                                      std::move(first_function_declaration_parameters),
		                                      BlockNode::Create(std::move(first_function_declaration_statements)));

		auto second_function_declaration_parameters = ASTNode::Dependencies{};
		auto second_function_declaration_statements = ASTNode::Dependencies{};
		auto first_function_call_parameters         = ASTNode::Dependencies{};
		first_function_call_parameters.reserve(2);
		first_function_call_parameters.emplace_back(
			LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::STRING>("my_string")));
		first_function_call_parameters.emplace_back(
			LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true)));
		second_function_declaration_statements.emplace_back(VariableDeclarationNode::Create(
			"variable",
			k_base_specifier_i32,
			FunctionCallNode::Create(k_function_to_call, std::move(first_function_call_parameters)),
			false));

		auto second_function_declaration
			= FunctionDeclarationNode::Create(k_function_name,
		                                      k_base_specifier_void,
		                                      std::move(second_function_declaration_parameters),
		                                      BlockNode::Create(std::move(second_function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(first_function_declaration));
		module_statements.push_back(std::move(second_function_declaration));
		auto result_ir = Build(ModuleNode::Create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.SetModuleName(k_module_name);
		expected_ir_builder.CreateFunction(k_function_to_call,
		                                   PrimitiveType::Kind::INT32,
		                                   { PrimitiveType::Kind::STRING, PrimitiveType::Kind::BOOLEAN });
		{
			auto* first_slot  = expected_ir_builder.ReserveSlot("a", PrimitiveType::Kind::STRING);
			auto* second_slot = expected_ir_builder.ReserveSlot("b", PrimitiveType::Kind::BOOLEAN);
			auto* first_arg   = expected_ir_builder.Emit<GetArgument>(PrimitiveType::Kind::STRING, 0);
			expected_ir_builder.Emit<StackStore>(first_slot, first_arg);
			auto* second_arg = expected_ir_builder.Emit<GetArgument>(PrimitiveType::Kind::BOOLEAN, 1);
			expected_ir_builder.Emit<StackStore>(second_slot, second_arg);
			auto* return_value = EmitConst<PrimitiveType::Kind::INT32>(expected_ir_builder, 1);
			expected_ir_builder.Emit<Return>(return_value);
		}

		expected_ir_builder.CreateFunction(k_function_name, PrimitiveType::Kind::VOID, {});
		auto* slot = expected_ir_builder.ReserveSlot("variable", PrimitiveType::Kind::INT32);
		auto* v1   = EmitConst<PrimitiveType::Kind::STRING>(expected_ir_builder, "my_string");
		auto* v2   = EmitConst<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, true);
		auto* value
			= expected_ir_builder.Emit<Call>(PrimitiveType::Kind::INT32, k_function_to_call, std::vector{ v1, v2 });
		expected_ir_builder.Emit<StackStore>(slot, value);

		const auto& expected_ir = expected_ir_builder.Build();

		auto [expected_string, result_string] = Compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, If)
	{
		auto if_node_condition  = LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true));
		auto if_then_statements = ASTNode::Dependencies{};
		if_then_statements.emplace_back(
			LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::STRING>("then_branch_string")));
		auto if_else_statements = ASTNode::Dependencies{};
		if_else_statements.emplace_back(LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(false)));

		auto if_node = IfNode::Create(std::move(if_node_condition),
		                              BlockNode::Create(std::move(if_then_statements)),
		                              BlockNode::Create(std::move(if_else_statements)));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.push_back(std::move(if_node));
		auto function_declaration
			= FunctionDeclarationNode::Create(k_function_name,
		                                      k_base_specifier_i32,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::Create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = Build(ModuleNode::Create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.SetModuleName(k_module_name);
		expected_ir_builder.CreateFunction(k_function_name, PrimitiveType::Kind::INT32, {});

		auto* input_block = expected_ir_builder.CurrentBasicBlock();
		expected_ir_builder.SwitchTo(input_block);
		auto* condition = EmitConst<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, true);

		auto* then_block = expected_ir_builder.CreateBasicBlock();
		auto* else_block = expected_ir_builder.CreateBasicBlock();
		expected_ir_builder.Emit<JumpIf>(condition, then_block, else_block);

		expected_ir_builder.SwitchTo(then_block);
		EmitConst<PrimitiveType::Kind::STRING>(expected_ir_builder, "then_branch_string");
		auto* output_block = expected_ir_builder.CreateBasicBlock();
		expected_ir_builder.Emit<Jump>(output_block);

		expected_ir_builder.SwitchTo(else_block);
		EmitConst<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, false);
		expected_ir_builder.Emit<Jump>(output_block);
		const auto& expected_ir = expected_ir_builder.Build();

		auto [expected_string, result_string] = Compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, Literals)
	{
		static constexpr std::array<std::pair<PrimitiveType::Kind, Value>, 7> k_input_values = {
			std::make_tuple(PrimitiveType::Kind::BOOLEAN, Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true)),
			std::make_tuple(PrimitiveType::Kind::CHAR, Scalar::Create<PrimitiveType::Kind::CHAR>('c')),
			std::make_tuple(PrimitiveType::Kind::FLOAT32, Scalar::Create<PrimitiveType::Kind::FLOAT32>(3.14f)),
			std::make_tuple(PrimitiveType::Kind::FLOAT64, Scalar::Create<PrimitiveType::Kind::FLOAT64>(5.46)),
			std::make_tuple(PrimitiveType::Kind::INT32, Scalar::Create<PrimitiveType::Kind::INT32>(123)),
			std::make_tuple(PrimitiveType::Kind::INT64, Scalar::Create<PrimitiveType::Kind::INT64>(456L)),
			std::make_tuple(PrimitiveType::Kind::STRING, Scalar::Create<PrimitiveType::Kind::STRING>("my_string")),
		};

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.reserve(k_input_values.size());
		for (const auto& value : k_input_values | std::views::values) {
			function_declaration_statements.emplace_back(LiteralNode::Create(value));
		}
		auto function_declaration
			= FunctionDeclarationNode::Create(k_function_name,
		                                      k_base_specifier_i32,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::Create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = Build(ModuleNode::Create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.SetModuleName(k_module_name);
		expected_ir_builder.CreateFunction(k_function_name, PrimitiveType::Kind::INT32, {});
		for (const auto& [type, value] : k_input_values) {
			expected_ir_builder.Emit<Const>(type, value);
		}
		const auto& expected_ir = expected_ir_builder.Build();

		auto [expected_string, result_string] = Compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, While)
	{
		static constexpr auto k_variable_name = "index";

		auto index_node
			= VariableDeclarationNode::Create(k_variable_name,
		                                      k_base_specifier_i32,
		                                      LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(0)),
		                                      true);

		auto while_node_condition
			= BinaryNode::Create(LiteralNode::Create(Value{ Identifier::create(k_variable_name) }),
		                         LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(10)),
		                         ASTNode::Operator::LESS);
		auto while_node_statements = ASTNode::Dependencies{};
		while_node_statements.emplace_back(
			BinaryNode::Create(LiteralNode::Create(Value{ Identifier::create(k_variable_name) }),
		                       BinaryNode::Create(LiteralNode::Create(Value{ Identifier::create(k_variable_name) }),
		                                          LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(1)),
		                                          ASTNode::Operator::ADD),
		                       ASTNode::Operator::ASSIGN));

		auto while_node
			= WhileNode::Create(std::move(while_node_condition), BlockNode::Create(std::move(while_node_statements)));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.push_back(std::move(index_node));
		function_declaration_statements.push_back(std::move(while_node));
		auto function_declaration
			= FunctionDeclarationNode::Create(k_function_name,
		                                      k_base_specifier_i32,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::Create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = Build(ModuleNode::Create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.SetModuleName(k_module_name);
		expected_ir_builder.CreateFunction(k_function_name, PrimitiveType::Kind::INT32, {});
		auto* input_block     = expected_ir_builder.CurrentBasicBlock();
		auto* condition_block = expected_ir_builder.CreateBasicBlock();
		auto* body_block      = expected_ir_builder.CreateBasicBlock();
		auto* output_block    = expected_ir_builder.CreateBasicBlock();

		expected_ir_builder.SwitchTo(input_block);
		{
			auto* slot          = expected_ir_builder.ReserveSlot(k_variable_name, PrimitiveType::Kind::INT32);
			auto* initial_value = EmitConst<PrimitiveType::Kind::INT32>(expected_ir_builder, 0);
			expected_ir_builder.Emit<StackStore>(slot, initial_value);
			expected_ir_builder.Emit<Jump>(condition_block);
		}

		expected_ir_builder.SwitchTo(condition_block);
		{
			auto* lhs       = expected_ir_builder.Emit<StackLoad>(expected_ir_builder.GetSlot(k_variable_name));
			auto* rhs       = EmitConst<PrimitiveType::Kind::INT32>(expected_ir_builder, 10);
			auto* condition = expected_ir_builder.Emit<Less>(lhs, rhs);
			expected_ir_builder.Emit<JumpIf>(condition, body_block, output_block);
		}

		expected_ir_builder.SwitchTo(body_block);
		{
			auto* slot       = expected_ir_builder.GetSlot(k_variable_name);
			auto* lhs        = expected_ir_builder.Emit<StackLoad>(slot);
			auto* rhs        = EmitConst<PrimitiveType::Kind::INT32>(expected_ir_builder, 1);
			auto* expression = expected_ir_builder.Emit<Add>(PrimitiveType::Kind::INT32, lhs, rhs);
			expected_ir_builder.Emit<StackStore>(slot, expression);
			expected_ir_builder.Emit<Jump>(condition_block);
		}
		const auto& expected_ir = expected_ir_builder.Build();

		auto [expected_string, result_string] = Compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, While_BreakAndContinue)
	{
		auto while_node_condition  = LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true));
		auto while_node_statements = ASTNode::Dependencies{};
		while_node_statements.reserve(2);
		while_node_statements.emplace_back(LoopControlNode::Create(LoopControlNode::Type::BREAK));
		while_node_statements.emplace_back(LoopControlNode::Create(LoopControlNode::Type::CONTINUE));
		auto while_node
			= WhileNode::Create(std::move(while_node_condition), BlockNode::Create(std::move(while_node_statements)));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.push_back(std::move(while_node));
		auto function_declaration
			= FunctionDeclarationNode::Create(k_function_name,
		                                      k_base_specifier_void,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::Create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = Build(ModuleNode::Create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.SetModuleName(k_module_name);
		expected_ir_builder.CreateFunction(k_function_name, PrimitiveType::Kind::VOID, {});
		auto* input_block     = expected_ir_builder.CurrentBasicBlock();
		auto* condition_block = expected_ir_builder.CreateBasicBlock();
		auto* body_block      = expected_ir_builder.CreateBasicBlock();
		auto* output_block    = expected_ir_builder.CreateBasicBlock();

		expected_ir_builder.SwitchTo(input_block);
		expected_ir_builder.Emit<Jump>(condition_block);

		expected_ir_builder.SwitchTo(condition_block);
		auto* condition = EmitConst<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, true);
		expected_ir_builder.Emit<JumpIf>(condition, body_block, output_block);

		expected_ir_builder.SwitchTo(body_block);
		expected_ir_builder.Emit<Jump>(output_block);     // Break
		expected_ir_builder.Emit<Jump>(condition_block);  // Continue
		expected_ir_builder.Emit<Jump>(condition_block);  // Fallthrough

		const auto& expected_ir = expected_ir_builder.Build();

		auto [expected_string, result_string] = Compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, While_NestedLoops)
	{
		auto inner_while_condition  = LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true));
		auto inner_while_statements = ASTNode::Dependencies{};
		auto inner_while_node
			= WhileNode::Create(std::move(inner_while_condition), BlockNode::Create(std::move(inner_while_statements)));

		auto outer_while_condition  = LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(false));
		auto outer_while_statements = ASTNode::Dependencies{};
		outer_while_statements.push_back(std::move(inner_while_node));
		auto outer_while_node
			= WhileNode::Create(std::move(outer_while_condition), BlockNode::Create(std::move(outer_while_statements)));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.push_back(std::move(outer_while_node));
		auto function_declaration
			= FunctionDeclarationNode::Create(k_function_name,
		                                      k_base_specifier_void,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::Create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = Build(ModuleNode::Create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.SetModuleName(k_module_name);
		expected_ir_builder.CreateFunction(k_function_name, PrimitiveType::Kind::VOID, {});

		auto* outer_while_input_block = expected_ir_builder.CurrentBasicBlock();
		expected_ir_builder.SwitchTo(outer_while_input_block);
		auto* outer_while_condition_block = expected_ir_builder.CreateBasicBlock();
		expected_ir_builder.Emit<Jump>(outer_while_condition_block);

		expected_ir_builder.SwitchTo(outer_while_condition_block);
		auto* outer_while_condition_value = EmitConst<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, false);
		auto* outer_while_body_block      = expected_ir_builder.CreateBasicBlock();
		auto* outer_while_output_block    = expected_ir_builder.CreateBasicBlock();
		expected_ir_builder.Emit<JumpIf>(outer_while_condition_value, outer_while_body_block, outer_while_output_block);

		expected_ir_builder.SwitchTo(outer_while_body_block);  // Becomes inner while's input block.
		auto* inner_while_condition_block = expected_ir_builder.CreateBasicBlock();
		expected_ir_builder.Emit<Jump>(inner_while_condition_block);

		expected_ir_builder.SwitchTo(inner_while_condition_block);
		auto* inner_while_condition_value = EmitConst<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, true);
		auto* inner_while_body_block      = expected_ir_builder.CreateBasicBlock();
		auto* inner_while_output_block    = expected_ir_builder.CreateBasicBlock();
		expected_ir_builder.Emit<JumpIf>(inner_while_condition_value, inner_while_body_block, inner_while_output_block);

		expected_ir_builder.SwitchTo(inner_while_body_block);
		expected_ir_builder.Emit<Jump>(inner_while_condition_block);

		expected_ir_builder.SwitchTo(inner_while_output_block);
		expected_ir_builder.Emit<Jump>(outer_while_condition_block);

		expected_ir_builder.SwitchTo(outer_while_output_block);

		const auto& expected_ir               = expected_ir_builder.Build();
		auto [expected_string, result_string] = Compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, Module_Empty)
	{
		IRBuilder expected_ir_builder{};
		expected_ir_builder.SetModuleName(k_module_name);
		const auto& result_ir = Build(ModuleNode::Create(k_module_name, ASTNode::Dependencies{}));
		ASSERT_TRUE(result_ir);

		const auto& expected_ir               = expected_ir_builder.Build();
		auto [expected_string, result_string] = Compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, Unary)
	{
		// NOTE: It doesn't matter that the actual value has invalid type.
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(UnaryNode::Create(
			LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::BOOLEAN>(true)), ASTNode::Operator::LOGICAL_NOT));
		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration
			= FunctionDeclarationNode::Create(this->k_function_name,
		                                      k_base_specifier_void,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::Create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = this->Build(ModuleNode::Create(this->k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.SetModuleName(this->k_module_name);
		expected_ir_builder.CreateFunction(this->k_function_name, PrimitiveType::Kind::VOID, {});
		auto* expression = EmitConst<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, true);
		expected_ir_builder.Emit<Not>(expression);
		const auto& expected_ir = expected_ir_builder.Build();
		ASSERT_TRUE(expected_ir);

		auto [expected_string, result_string] = this->Compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, VariableDeclaration_SimpleDeclarationAndAssignment)
	{
		static constexpr auto k_first_variable_name  = "first_variable";
		static constexpr auto k_second_variable_name = "second_variable";

		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.reserve(4);
		function_declaration_statements.emplace_back(
			VariableDeclarationNode::Create(k_first_variable_name,
		                                    k_base_specifier_i32,
		                                    LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(1)),
		                                    true));
		function_declaration_statements.emplace_back(
			BinaryNode::Create(LiteralNode::Create(Identifier::create(k_first_variable_name)),
		                       LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(3)),
		                       ASTNode::Operator::ASSIGN));
		function_declaration_statements.emplace_back(
			VariableDeclarationNode::Create(k_second_variable_name,
		                                    k_base_specifier_i32,
		                                    LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(5)),
		                                    false));
		function_declaration_statements.emplace_back(
			BinaryNode::Create(LiteralNode::Create(Identifier::create(k_second_variable_name)),
		                       LiteralNode::Create(Identifier::create(k_first_variable_name)),
		                       ASTNode::Operator::ASSIGN));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration
			= FunctionDeclarationNode::Create(k_function_name,
		                                      k_base_specifier_void,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::Create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = Build(ModuleNode::Create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.SetModuleName(k_module_name);
		expected_ir_builder.CreateFunction(k_function_name, PrimitiveType::Kind::VOID, {});
		auto* first_slot  = expected_ir_builder.ReserveSlot(k_first_variable_name, PrimitiveType::Kind::INT32);
		auto* second_slot = expected_ir_builder.ReserveSlot(k_second_variable_name, PrimitiveType::Kind::INT32);

		expected_ir_builder.Emit<StackStore>(first_slot, EmitConst<PrimitiveType::Kind::INT32>(expected_ir_builder, 1));
		expected_ir_builder.Emit<StackStore>(first_slot, EmitConst<PrimitiveType::Kind::INT32>(expected_ir_builder, 3));
		expected_ir_builder.Emit<StackStore>(second_slot,
		                                     EmitConst<PrimitiveType::Kind::INT32>(expected_ir_builder, 5));
		auto* first_slot_value = expected_ir_builder.Emit<StackLoad>(first_slot);
		expected_ir_builder.Emit<StackStore>(second_slot, first_slot_value);

		auto expected_ir = expected_ir_builder.Build();
		ASSERT_TRUE(expected_ir);

		auto [expected_string, result_string] = Compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, VariableDeclaration_ReadTheValueTwice)
	{
		static constexpr auto k_first_variable_name  = "first_variable";
		static constexpr auto k_second_variable_name = "second_variable";

		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.reserve(2);
		function_declaration_statements.emplace_back(
			VariableDeclarationNode::Create(k_first_variable_name,
		                                    k_base_specifier_i32,
		                                    LiteralNode::Create(Scalar::Create<PrimitiveType::Kind::INT32>(1)),
		                                    false));
		function_declaration_statements.emplace_back(VariableDeclarationNode::Create(
			k_second_variable_name,
			k_base_specifier_i32,
			BinaryNode::Create(LiteralNode::Create(Identifier::create(k_first_variable_name)),
		                       LiteralNode::Create(Identifier::create(k_first_variable_name)),
		                       ASTNode::Operator::MUL),
			false));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration
			= FunctionDeclarationNode::Create(k_function_name,
		                                      k_base_specifier_void,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::Create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = Build(ModuleNode::Create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.SetModuleName(k_module_name);
		expected_ir_builder.CreateFunction(k_function_name, PrimitiveType::Kind::VOID, {});
		auto* first_slot  = expected_ir_builder.ReserveSlot(k_first_variable_name, PrimitiveType::Kind::INT32);
		auto* second_slot = expected_ir_builder.ReserveSlot(k_second_variable_name, PrimitiveType::Kind::INT32);
		auto* value       = EmitConst<PrimitiveType::Kind::INT32>(expected_ir_builder, 1);
		expected_ir_builder.Emit<StackStore>(first_slot, value);
		auto* lhs    = expected_ir_builder.Emit<StackLoad>(first_slot);
		auto* rhs    = expected_ir_builder.Emit<StackLoad>(first_slot);
		auto* result = expected_ir_builder.Emit<Mul>(PrimitiveType::Kind::INT32, lhs, rhs);
		expected_ir_builder.Emit<StackStore>(second_slot, result);
		auto expected_ir = expected_ir_builder.Build();
		ASSERT_TRUE(expected_ir);

		auto [expected_string, result_string] = Compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	template <typename>
	class LowerVisitorBinaryTypedTest : public LowerVisitorTest
	{
	};

	template <ASTNode::Operator Op, PrimitiveType::Kind Type>
	struct BinaryCase
	{
		static constexpr auto k_operator = Op;
		static constexpr auto k_type     = Type;
	};

	template <ASTNode::Operator Op>
	using BinaryInstruction = VTDictionary<Op,
	                                       VTTranslation<ASTNode::Operator::ADD, Add>,
	                                       VTTranslation<ASTNode::Operator::SUB, Sub>,
	                                       VTTranslation<ASTNode::Operator::MUL, Mul>,
	                                       VTTranslation<ASTNode::Operator::DIV, Div>,
	                                       VTTranslation<ASTNode::Operator::MOD, Mod>,
	                                       VTTranslation<ASTNode::Operator::EQUAL, Equal>,
	                                       VTTranslation<ASTNode::Operator::NOT_EQUAL, NotEqual>,
	                                       VTTranslation<ASTNode::Operator::GREATER, Greater>,
	                                       VTTranslation<ASTNode::Operator::GREATER_EQUAL, GreaterEqual>,
	                                       VTTranslation<ASTNode::Operator::LESS, Less>,
	                                       VTTranslation<ASTNode::Operator::LESS_EQUAL, LessEqual>,
	                                       VTTranslation<ASTNode::Operator::LOGICAL_AND, And>,
	                                       VTTranslation<ASTNode::Operator::LOGICAL_OR, Or>>::Type;

	using BinaryTypes = ::testing::Types<BinaryCase<ASTNode::Operator::ADD, PrimitiveType::Kind::INT32>,
	                                     BinaryCase<ASTNode::Operator::SUB, PrimitiveType::Kind::INT32>,
	                                     BinaryCase<ASTNode::Operator::MUL, PrimitiveType::Kind::INT32>,
	                                     BinaryCase<ASTNode::Operator::DIV, PrimitiveType::Kind::INT32>,
	                                     BinaryCase<ASTNode::Operator::MOD, PrimitiveType::Kind::INT32>,
	                                     BinaryCase<ASTNode::Operator::EQUAL, PrimitiveType::Kind::FLOAT32>,
	                                     BinaryCase<ASTNode::Operator::NOT_EQUAL, PrimitiveType::Kind::FLOAT32>,
	                                     BinaryCase<ASTNode::Operator::GREATER, PrimitiveType::Kind::FLOAT32>,
	                                     BinaryCase<ASTNode::Operator::GREATER_EQUAL, PrimitiveType::Kind::FLOAT32>,
	                                     BinaryCase<ASTNode::Operator::LESS, PrimitiveType::Kind::FLOAT32>,
	                                     BinaryCase<ASTNode::Operator::LESS_EQUAL, PrimitiveType::Kind::FLOAT32>,
	                                     BinaryCase<ASTNode::Operator::LOGICAL_AND, PrimitiveType::Kind::BOOLEAN>,
	                                     BinaryCase<ASTNode::Operator::LOGICAL_OR, PrimitiveType::Kind::BOOLEAN>>;
	TYPED_TEST_SUITE(LowerVisitorBinaryTypedTest, BinaryTypes);

	TYPED_TEST(LowerVisitorBinaryTypedTest, Binary)
	{
		using InstructionCase        = BinaryInstruction<TypeParam::k_operator>;
		static constexpr auto k_type = TypeParam::k_type;

		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(BinaryNode::Create(LiteralNode::Create(Scalar::Create<k_type>(1)),
		                                                                LiteralNode::Create(Scalar::Create<k_type>(2)),
		                                                                TypeParam::k_operator));
		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration
			= FunctionDeclarationNode::Create(this->k_function_name,
		                                      k_base_specifier_void,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::Create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = this->Build(ModuleNode::Create(this->k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.SetModuleName(this->k_module_name);
		expected_ir_builder.CreateFunction(this->k_function_name, PrimitiveType::Kind::VOID, {});
		auto* v1 = expected_ir_builder.Emit<Const>(k_type, Scalar::Create<k_type>(1));
		auto* v2 = expected_ir_builder.Emit<Const>(k_type, Scalar::Create<k_type>(2));
		if constexpr (std::is_constructible_v<InstructionCase, Types::Type, Instruction*, Instruction*>) {
			expected_ir_builder.Emit<InstructionCase>(k_type, v1, v2);
		} else if constexpr (std::is_constructible_v<InstructionCase, Instruction*, Instruction*>) {
			expected_ir_builder.Emit<InstructionCase>(v1, v2);
		} else {
			GTEST_FAIL() << "unhandled instruction case";
		}
		auto expected_ir = expected_ir_builder.Build();
		ASSERT_TRUE(expected_ir);

		auto [expected_string, result_string] = this->Compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}
}  // namespace Soul::AST::Visitors::UT
