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
		// NOLINTNEXTLINE(bugprone-easily-swappable-parameters)
		std::pair<std::string, std::string> compare(const Module& expected, const Module& result)
		{
			IR::Visitors::PrintVisitor print_result{};
			print_result.accept(result);

			IR::Visitors::PrintVisitor print_expected{};
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

		template <PrimitiveType::Kind T>
		IR::Instruction* emit_const(IR::IRBuilder& builder, detail::PrimitiveKindToValueType<T> value)
		{
			return builder.emit<Const>(T, Scalar::create<T>(std::move(value)));
		}
	};

	TEST_F(LowerVisitorTest, Block)
	{
		auto inner_scope_statements = ASTNode::Dependencies{};
		inner_scope_statements.reserve(3);
		inner_scope_statements.emplace_back(LiteralNode::create(Scalar::create<PrimitiveType::Kind::BOOLEAN>(true)));
		inner_scope_statements.emplace_back(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::STRING>("my_string")));
		inner_scope_statements.emplace_back(LiteralNode::create(Scalar::create<PrimitiveType::Kind::INT64>(123)));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(BlockNode::create(std::move(inner_scope_statements)));
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      k_base_specifier_i32,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, PrimitiveType::Kind::INT32, {});
		auto* inner_block = expected_ir_builder.create_basic_block();
		expected_ir_builder.emit<Jump>(inner_block);
		expected_ir_builder.switch_to(inner_block);
		emit_const<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, true);
		emit_const<PrimitiveType::Kind::STRING>(expected_ir_builder, "my_string");
		emit_const<PrimitiveType::Kind::INT64>(expected_ir_builder, 123);
		const auto& expected_ir = expected_ir_builder.build();

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, Cast)
	{
		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(CastNode::create(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::INT32>(123)), k_base_specifier_str));
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      k_base_specifier_i32,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, PrimitiveType::Kind::INT32, {});
		auto* instr = emit_const<PrimitiveType::Kind::INT32>(expected_ir_builder, 123);
		expected_ir_builder.emit<Cast>(PrimitiveType::Kind::STRING, instr);
		const auto& expected_ir = expected_ir_builder.build();

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, FunctionCall)
	{
		static constexpr auto k_function_to_call = "function_to_be_called";

		auto first_function_declaration_parameters = ASTNode::Dependencies{};
		first_function_declaration_parameters.reserve(2);
		first_function_declaration_parameters.emplace_back(
			VariableDeclarationNode::create("a", k_base_specifier_str, nullptr, true));
		first_function_declaration_parameters.emplace_back(
			VariableDeclarationNode::create("b", k_base_specifier_bool, nullptr, true));
		auto first_function_declaration_statements = ASTNode::Dependencies{};
		first_function_declaration_statements.emplace_back(
			ReturnNode::create(LiteralNode::create(Scalar::create<PrimitiveType::Kind::INT32>(1))));
		auto first_function_declaration
			= FunctionDeclarationNode::create(k_function_to_call,
		                                      k_base_specifier_i32,
		                                      std::move(first_function_declaration_parameters),
		                                      BlockNode::create(std::move(first_function_declaration_statements)));

		auto second_function_declaration_parameters = ASTNode::Dependencies{};
		auto second_function_declaration_statements = ASTNode::Dependencies{};
		auto first_function_call_parameters         = ASTNode::Dependencies{};
		first_function_call_parameters.reserve(2);
		first_function_call_parameters.emplace_back(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::STRING>("my_string")));
		first_function_call_parameters.emplace_back(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::BOOLEAN>(true)));
		second_function_declaration_statements.emplace_back(VariableDeclarationNode::create(
			"variable",
			k_base_specifier_i32,
			FunctionCallNode::create(k_function_to_call, std::move(first_function_call_parameters)),
			false));

		auto second_function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      k_base_specifier_void,
		                                      std::move(second_function_declaration_parameters),
		                                      BlockNode::create(std::move(second_function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(first_function_declaration));
		module_statements.push_back(std::move(second_function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_to_call,
		                                    PrimitiveType::Kind::INT32,
		                                    { PrimitiveType::Kind::STRING, PrimitiveType::Kind::BOOLEAN });
		{
			auto* first_slot  = expected_ir_builder.reserve_slot("a", PrimitiveType::Kind::STRING);
			auto* second_slot = expected_ir_builder.reserve_slot("b", PrimitiveType::Kind::BOOLEAN);
			auto* first_arg   = expected_ir_builder.emit<GetArgument>(PrimitiveType::Kind::STRING, 0);
			expected_ir_builder.emit<StackStore>(first_slot, first_arg);
			auto* second_arg = expected_ir_builder.emit<GetArgument>(PrimitiveType::Kind::BOOLEAN, 1);
			expected_ir_builder.emit<StackStore>(second_slot, second_arg);
			auto* return_value = emit_const<PrimitiveType::Kind::INT32>(expected_ir_builder, 1);
			expected_ir_builder.emit<Return>(return_value);
		}

		expected_ir_builder.create_function(k_function_name, PrimitiveType::Kind::VOID, {});
		auto* slot  = expected_ir_builder.reserve_slot("variable", PrimitiveType::Kind::INT32);
		auto* v1    = emit_const<PrimitiveType::Kind::STRING>(expected_ir_builder, "my_string");
		auto* v2    = emit_const<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, true);
		auto* value = expected_ir_builder.emit<Call>(
			PrimitiveType::Kind::INT32, k_function_to_call, std::vector<Instruction*>{ v1, v2 });
		expected_ir_builder.emit<StackStore>(slot, value);

		const auto& expected_ir = expected_ir_builder.build();

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, If)
	{
		auto if_node_condition  = LiteralNode::create(Scalar::create<PrimitiveType::Kind::BOOLEAN>(true));
		auto if_then_statements = ASTNode::Dependencies{};
		if_then_statements.emplace_back(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::STRING>("then_branch_string")));
		auto if_else_statements = ASTNode::Dependencies{};
		if_else_statements.emplace_back(LiteralNode::create(Scalar::create<PrimitiveType::Kind::BOOLEAN>(false)));

		auto if_node = IfNode::create(std::move(if_node_condition),
		                              BlockNode::create(std::move(if_then_statements)),
		                              BlockNode::create(std::move(if_else_statements)));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.push_back(std::move(if_node));
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      k_base_specifier_i32,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, PrimitiveType::Kind::INT32, {});

		auto* input_block = expected_ir_builder.current_basic_block();
		expected_ir_builder.switch_to(input_block);
		auto* condition = emit_const<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, true);

		auto* then_block = expected_ir_builder.create_basic_block();
		auto* else_block = expected_ir_builder.create_basic_block();
		expected_ir_builder.emit<JumpIf>(condition, then_block, else_block);

		expected_ir_builder.switch_to(then_block);
		emit_const<PrimitiveType::Kind::STRING>(expected_ir_builder, "then_branch_string");
		auto* output_block = expected_ir_builder.create_basic_block();
		expected_ir_builder.emit<Jump>(output_block);

		expected_ir_builder.switch_to(else_block);
		emit_const<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, false);
		expected_ir_builder.emit<Jump>(output_block);
		const auto& expected_ir = expected_ir_builder.build();

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, Literals)
	{
		static constexpr std::array<std::pair<PrimitiveType::Kind, Value>, 7> k_input_values = {
			std::make_tuple(PrimitiveType::Kind::BOOLEAN, Scalar::create<PrimitiveType::Kind::BOOLEAN>(true)),
			std::make_tuple(PrimitiveType::Kind::CHAR, Scalar::create<PrimitiveType::Kind::CHAR>('c')),
			std::make_tuple(PrimitiveType::Kind::FLOAT32, Scalar::create<PrimitiveType::Kind::FLOAT32>(3.14f)),
			std::make_tuple(PrimitiveType::Kind::FLOAT64, Scalar::create<PrimitiveType::Kind::FLOAT64>(5.46)),
			std::make_tuple(PrimitiveType::Kind::INT32, Scalar::create<PrimitiveType::Kind::INT32>(123)),
			std::make_tuple(PrimitiveType::Kind::INT64, Scalar::create<PrimitiveType::Kind::INT64>(456L)),
			std::make_tuple(PrimitiveType::Kind::STRING, Scalar::create<PrimitiveType::Kind::STRING>("my_string")),
		};

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.reserve(k_input_values.size());
		for (const auto& [_, value] : k_input_values) {
			function_declaration_statements.emplace_back(LiteralNode::create(value));
		}
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      k_base_specifier_i32,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, PrimitiveType::Kind::INT32, {});
		for (const auto& [type, value] : k_input_values) {
			expected_ir_builder.emit<Const>(type, value);
		}
		const auto& expected_ir = expected_ir_builder.build();

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, While)
	{
		static constexpr auto k_variable_name = "index";

		auto index_node
			= VariableDeclarationNode::create(k_variable_name,
		                                      k_base_specifier_i32,
		                                      LiteralNode::create(Scalar::create<PrimitiveType::Kind::INT32>(0)),
		                                      true);

		auto while_node_condition
			= BinaryNode::create(LiteralNode::create(Value{ Identifier::create(k_variable_name) }),
		                         LiteralNode::create(Scalar::create<PrimitiveType::Kind::INT32>(10)),
		                         ASTNode::Operator::Less);
		auto while_node_statements = ASTNode::Dependencies{};
		while_node_statements.emplace_back(
			BinaryNode::create(LiteralNode::create(Value{ Identifier::create(k_variable_name) }),
		                       BinaryNode::create(LiteralNode::create(Value{ Identifier::create(k_variable_name) }),
		                                          LiteralNode::create(Scalar::create<PrimitiveType::Kind::INT32>(1)),
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
		                                      k_base_specifier_i32,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, PrimitiveType::Kind::INT32, {});
		auto* input_block     = expected_ir_builder.current_basic_block();
		auto* condition_block = expected_ir_builder.create_basic_block();
		auto* body_block      = expected_ir_builder.create_basic_block();
		auto* output_block    = expected_ir_builder.create_basic_block();

		expected_ir_builder.switch_to(input_block);
		{
			auto* slot          = expected_ir_builder.reserve_slot(k_variable_name, PrimitiveType::Kind::INT32);
			auto* initial_value = emit_const<PrimitiveType::Kind::INT32>(expected_ir_builder, 0);
			expected_ir_builder.emit<StackStore>(slot, initial_value);
			expected_ir_builder.emit<Jump>(condition_block);
		}

		expected_ir_builder.switch_to(condition_block);
		{
			auto* lhs       = expected_ir_builder.emit<StackLoad>(expected_ir_builder.get_slot(k_variable_name));
			auto* rhs       = emit_const<PrimitiveType::Kind::INT32>(expected_ir_builder, 10);
			auto* condition = expected_ir_builder.emit<Less>(lhs, rhs);
			expected_ir_builder.emit<JumpIf>(condition, body_block, output_block);
		}

		expected_ir_builder.switch_to(body_block);
		{
			auto* slot       = expected_ir_builder.get_slot(k_variable_name);
			auto* lhs        = expected_ir_builder.emit<StackLoad>(slot);
			auto* rhs        = emit_const<PrimitiveType::Kind::INT32>(expected_ir_builder, 1);
			auto* expression = expected_ir_builder.emit<Add>(PrimitiveType::Kind::INT32, lhs, rhs);
			expected_ir_builder.emit<StackStore>(slot, expression);
			expected_ir_builder.emit<Jump>(condition_block);
		}
		const auto& expected_ir = expected_ir_builder.build();

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, While_BreakAndContinue)
	{
		auto while_node_condition  = LiteralNode::create(Scalar::create<PrimitiveType::Kind::BOOLEAN>(true));
		auto while_node_statements = ASTNode::Dependencies{};
		while_node_statements.reserve(2);
		while_node_statements.emplace_back(LoopControlNode::create(LoopControlNode::Type::Break));
		while_node_statements.emplace_back(LoopControlNode::create(LoopControlNode::Type::Continue));
		auto while_node
			= WhileNode::create(std::move(while_node_condition), BlockNode::create(std::move(while_node_statements)));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.push_back(std::move(while_node));
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      k_base_specifier_void,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, PrimitiveType::Kind::VOID, {});
		auto* input_block     = expected_ir_builder.current_basic_block();
		auto* condition_block = expected_ir_builder.create_basic_block();
		auto* body_block      = expected_ir_builder.create_basic_block();
		auto* output_block    = expected_ir_builder.create_basic_block();

		expected_ir_builder.switch_to(input_block);
		expected_ir_builder.emit<Jump>(condition_block);

		expected_ir_builder.switch_to(condition_block);
		auto* condition = emit_const<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, true);
		expected_ir_builder.emit<JumpIf>(condition, body_block, output_block);

		expected_ir_builder.switch_to(body_block);
		expected_ir_builder.emit<Jump>(output_block);     // Break
		expected_ir_builder.emit<Jump>(condition_block);  // Continue
		expected_ir_builder.emit<Jump>(condition_block);  // Fallthrough

		const auto& expected_ir = expected_ir_builder.build();

		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, While_NestedLoops)
	{
		auto inner_while_condition  = LiteralNode::create(Scalar::create<PrimitiveType::Kind::BOOLEAN>(true));
		auto inner_while_statements = ASTNode::Dependencies{};
		auto inner_while_node
			= WhileNode::create(std::move(inner_while_condition), BlockNode::create(std::move(inner_while_statements)));

		auto outer_while_condition  = LiteralNode::create(Scalar::create<PrimitiveType::Kind::BOOLEAN>(false));
		auto outer_while_statements = ASTNode::Dependencies{};
		outer_while_statements.push_back(std::move(inner_while_node));
		auto outer_while_node
			= WhileNode::create(std::move(outer_while_condition), BlockNode::create(std::move(outer_while_statements)));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.push_back(std::move(outer_while_node));
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      k_base_specifier_void,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, PrimitiveType::Kind::VOID, {});

		auto* outer_while_input_block = expected_ir_builder.current_basic_block();
		expected_ir_builder.switch_to(outer_while_input_block);
		auto* outer_while_condition_block = expected_ir_builder.create_basic_block();
		expected_ir_builder.emit<Jump>(outer_while_condition_block);

		expected_ir_builder.switch_to(outer_while_condition_block);
		auto* outer_while_condition_value = emit_const<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, false);
		auto* outer_while_body_block      = expected_ir_builder.create_basic_block();
		auto* outer_while_output_block    = expected_ir_builder.create_basic_block();
		expected_ir_builder.emit<JumpIf>(outer_while_condition_value, outer_while_body_block, outer_while_output_block);

		expected_ir_builder.switch_to(outer_while_body_block);  // Becomes inner while's input block.
		auto* inner_while_condition_block = expected_ir_builder.create_basic_block();
		expected_ir_builder.emit<Jump>(inner_while_condition_block);

		expected_ir_builder.switch_to(inner_while_condition_block);
		auto* inner_while_condition_value = emit_const<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, true);
		auto* inner_while_body_block      = expected_ir_builder.create_basic_block();
		auto* inner_while_output_block    = expected_ir_builder.create_basic_block();
		expected_ir_builder.emit<JumpIf>(inner_while_condition_value, inner_while_body_block, inner_while_output_block);

		expected_ir_builder.switch_to(inner_while_body_block);
		expected_ir_builder.emit<Jump>(inner_while_condition_block);

		expected_ir_builder.switch_to(inner_while_output_block);
		expected_ir_builder.emit<Jump>(outer_while_condition_block);

		expected_ir_builder.switch_to(outer_while_output_block);

		const auto& expected_ir               = expected_ir_builder.build();
		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, Module_Empty)
	{
		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		const auto& result_ir = build(ModuleNode::create(k_module_name, ASTNode::Dependencies{}));
		ASSERT_TRUE(result_ir);

		const auto& expected_ir               = expected_ir_builder.build();
		auto [expected_string, result_string] = compare(*expected_ir, *result_ir);
		ASSERT_EQ(expected_string, result_string);
	}

	TEST_F(LowerVisitorTest, Unary)
	{
		// NOTE: It doesn't matter that the actual value has invalid type.
		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(UnaryNode::create(
			LiteralNode::create(Scalar::create<PrimitiveType::Kind::BOOLEAN>(true)), ASTNode::Operator::LogicalNot));
		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration
			= FunctionDeclarationNode::create(this->k_function_name,
		                                      k_base_specifier_void,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = this->build(ModuleNode::create(this->k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(this->k_module_name);
		expected_ir_builder.create_function(this->k_function_name, PrimitiveType::Kind::VOID, {});
		auto* expression = emit_const<PrimitiveType::Kind::BOOLEAN>(expected_ir_builder, true);
		expected_ir_builder.emit<Not>(expression);
		const auto& expected_ir = expected_ir_builder.build();
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
		function_declaration_statements.emplace_back(
			VariableDeclarationNode::create(k_first_variable_name,
		                                    k_base_specifier_i32,
		                                    LiteralNode::create(Scalar::create<PrimitiveType::Kind::INT32>(1)),
		                                    true));
		function_declaration_statements.emplace_back(
			BinaryNode::create(LiteralNode::create(Identifier::create(k_first_variable_name)),
		                       LiteralNode::create(Scalar::create<PrimitiveType::Kind::INT32>(3)),
		                       ASTNode::Operator::Assign));
		function_declaration_statements.emplace_back(
			VariableDeclarationNode::create(k_second_variable_name,
		                                    k_base_specifier_i32,
		                                    LiteralNode::create(Scalar::create<PrimitiveType::Kind::INT32>(5)),
		                                    false));
		function_declaration_statements.emplace_back(
			BinaryNode::create(LiteralNode::create(Identifier::create(k_second_variable_name)),
		                       LiteralNode::create(Identifier::create(k_first_variable_name)),
		                       ASTNode::Operator::Assign));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      k_base_specifier_void,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, PrimitiveType::Kind::VOID, {});
		auto* first_slot  = expected_ir_builder.reserve_slot(k_first_variable_name, PrimitiveType::Kind::INT32);
		auto* second_slot = expected_ir_builder.reserve_slot(k_second_variable_name, PrimitiveType::Kind::INT32);

		expected_ir_builder.emit<StackStore>(first_slot,
		                                     emit_const<PrimitiveType::Kind::INT32>(expected_ir_builder, 1));
		expected_ir_builder.emit<StackStore>(first_slot,
		                                     emit_const<PrimitiveType::Kind::INT32>(expected_ir_builder, 3));
		expected_ir_builder.emit<StackStore>(second_slot,
		                                     emit_const<PrimitiveType::Kind::INT32>(expected_ir_builder, 5));
		auto* first_slot_value = expected_ir_builder.emit<StackLoad>(first_slot);
		expected_ir_builder.emit<StackStore>(second_slot, first_slot_value);

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
		function_declaration_statements.emplace_back(
			VariableDeclarationNode::create(k_first_variable_name,
		                                    k_base_specifier_i32,
		                                    LiteralNode::create(Scalar::create<PrimitiveType::Kind::INT32>(1)),
		                                    false));
		function_declaration_statements.emplace_back(VariableDeclarationNode::create(
			k_second_variable_name,
			k_base_specifier_i32,
			BinaryNode::create(LiteralNode::create(Identifier::create(k_first_variable_name)),
		                       LiteralNode::create(Identifier::create(k_first_variable_name)),
		                       ASTNode::Operator::Mul),
			false));

		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration
			= FunctionDeclarationNode::create(k_function_name,
		                                      k_base_specifier_void,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = build(ModuleNode::create(k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(k_module_name);
		expected_ir_builder.create_function(k_function_name, PrimitiveType::Kind::VOID, {});
		auto* first_slot  = expected_ir_builder.reserve_slot(k_first_variable_name, PrimitiveType::Kind::INT32);
		auto* second_slot = expected_ir_builder.reserve_slot(k_second_variable_name, PrimitiveType::Kind::INT32);
		auto* value       = emit_const<PrimitiveType::Kind::INT32>(expected_ir_builder, 1);
		expected_ir_builder.emit<StackStore>(first_slot, value);
		auto* lhs    = expected_ir_builder.emit<StackLoad>(first_slot);
		auto* rhs    = expected_ir_builder.emit<StackLoad>(first_slot);
		auto* result = expected_ir_builder.emit<Mul>(PrimitiveType::Kind::INT32, lhs, rhs);
		expected_ir_builder.emit<StackStore>(second_slot, result);
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

	template <ASTNode::Operator Op>
	using BinaryInstruction = VTDictionary<Op,
	                                       VTTranslation<ASTNode::Operator::Add, IR::Add>,
	                                       VTTranslation<ASTNode::Operator::Sub, IR::Sub>,
	                                       VTTranslation<ASTNode::Operator::Mul, IR::Mul>,
	                                       VTTranslation<ASTNode::Operator::Div, IR::Div>,
	                                       VTTranslation<ASTNode::Operator::Mod, IR::Mod>,
	                                       VTTranslation<ASTNode::Operator::Equal, IR::Equal>,
	                                       VTTranslation<ASTNode::Operator::NotEqual, IR::NotEqual>,
	                                       VTTranslation<ASTNode::Operator::Greater, IR::Greater>,
	                                       VTTranslation<ASTNode::Operator::GreaterEqual, IR::GreaterEqual>,
	                                       VTTranslation<ASTNode::Operator::Less, IR::Less>,
	                                       VTTranslation<ASTNode::Operator::LessEqual, IR::LessEqual>,
	                                       VTTranslation<ASTNode::Operator::LogicalAnd, IR::And>,
	                                       VTTranslation<ASTNode::Operator::LogicalOr, IR::Or>>::Type;

	using BinaryTypes = ::testing::Types<BinaryCase<ASTNode::Operator::Add, PrimitiveType::Kind::INT32>,
	                                     BinaryCase<ASTNode::Operator::Sub, PrimitiveType::Kind::INT32>,
	                                     BinaryCase<ASTNode::Operator::Mul, PrimitiveType::Kind::INT32>,
	                                     BinaryCase<ASTNode::Operator::Div, PrimitiveType::Kind::INT32>,
	                                     BinaryCase<ASTNode::Operator::Mod, PrimitiveType::Kind::INT32>,
	                                     BinaryCase<ASTNode::Operator::Equal, PrimitiveType::Kind::FLOAT32>,
	                                     BinaryCase<ASTNode::Operator::NotEqual, PrimitiveType::Kind::FLOAT32>,
	                                     BinaryCase<ASTNode::Operator::Greater, PrimitiveType::Kind::FLOAT32>,
	                                     BinaryCase<ASTNode::Operator::GreaterEqual, PrimitiveType::Kind::FLOAT32>,
	                                     BinaryCase<ASTNode::Operator::Less, PrimitiveType::Kind::FLOAT32>,
	                                     BinaryCase<ASTNode::Operator::LessEqual, PrimitiveType::Kind::FLOAT32>,
	                                     BinaryCase<ASTNode::Operator::LogicalAnd, PrimitiveType::Kind::BOOLEAN>,
	                                     BinaryCase<ASTNode::Operator::LogicalOr, PrimitiveType::Kind::BOOLEAN>>;
	TYPED_TEST_SUITE(LowerVisitorBinaryTypedTest, BinaryTypes);

	TYPED_TEST(LowerVisitorBinaryTypedTest, Binary)
	{
		using InstructionCase        = BinaryInstruction<TypeParam::k_operator>;
		static constexpr auto k_type = TypeParam::k_type;

		auto function_declaration_statements = ASTNode::Dependencies{};
		function_declaration_statements.emplace_back(BinaryNode::create(LiteralNode::create(Scalar::create<k_type>(1)),
		                                                                LiteralNode::create(Scalar::create<k_type>(2)),
		                                                                TypeParam::k_operator));
		auto function_declaration_parameters = ASTNode::Dependencies{};
		auto function_declaration
			= FunctionDeclarationNode::create(this->k_function_name,
		                                      k_base_specifier_void,
		                                      std::move(function_declaration_parameters),
		                                      BlockNode::create(std::move(function_declaration_statements)));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.push_back(std::move(function_declaration));
		auto result_ir = this->build(ModuleNode::create(this->k_module_name, std::move(module_statements)));
		ASSERT_TRUE(result_ir);

		IRBuilder expected_ir_builder{};
		expected_ir_builder.set_module_name(this->k_module_name);
		expected_ir_builder.create_function(this->k_function_name, PrimitiveType::Kind::VOID, {});
		auto* v1 = expected_ir_builder.emit<Const>(k_type, Scalar::create<k_type>(1));
		auto* v2 = expected_ir_builder.emit<Const>(k_type, Scalar::create<k_type>(2));
		if constexpr (std::is_constructible_v<InstructionCase, Types::Type, Instruction*, Instruction*>) {
			expected_ir_builder.emit<InstructionCase>(k_type, v1, v2);
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
}  // namespace Soul::AST::Visitors::UT
