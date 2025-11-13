#include "ast/visitors/type_discoverer.h"

#include <gtest/gtest.h>

#include "ast/ast.h"
#include "ast/visitors/stringify.h"

#include <format>

namespace soul::ast::visitors::ut
{
	using namespace soul::types;
	using namespace std::string_view_literals;

	class TypeDiscovererTest : public ::testing::Test
	{
	};

	TEST_F(TypeDiscovererTest, StructUsedByOtherStruct)
	{
		// Prepare the types...
		auto first_struct_members = ASTNode::Dependencies{};
		first_struct_members.reserve(3);
		first_struct_members.emplace_back(VariableDeclarationNode::create("my_int", "i32", nullptr, false));
		first_struct_members.emplace_back(VariableDeclarationNode::create("my_float", "f64", nullptr, false));
		first_struct_members.emplace_back(VariableDeclarationNode::create("my_string", "str", nullptr, false));
		auto first_struct = StructDeclarationNode::create("first_struct", std::move(first_struct_members));

		auto second_struct_members = ASTNode::Dependencies{};
		second_struct_members.reserve(2);
		second_struct_members.emplace_back(
			VariableDeclarationNode::create("my_struct", "first_struct", nullptr, false));
		second_struct_members.emplace_back(VariableDeclarationNode::create("my_bool", "bool", nullptr, false));
		auto second_struct = StructDeclarationNode::create("second_struct", std::move(second_struct_members));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(std::move(first_struct));
		module_statements.emplace_back(std::move(second_struct));
		auto expected_module = ModuleNode::create("discovery_module", std::move(module_statements));

		// ...discover them...
		TypeDiscovererVisitor type_discoverer{};
		type_discoverer.accept(expected_module.get());

		// ...and verify the results.
		auto first_struct_type  = Type{ StructType{
			 { PrimitiveType::Kind::Int32, PrimitiveType::Kind::Float64, PrimitiveType::Kind::String } } };
		auto second_struct_type = Type{ StructType{ { first_struct_type, PrimitiveType::Kind::Boolean } } };

		auto k_expected_types               = TypeDiscovererVisitor::basic_types();
		k_expected_types["first_struct"sv]  = first_struct_type;
		k_expected_types["second_struct"sv] = second_struct_type;
		ASSERT_EQ(k_expected_types, type_discoverer.discovered_types());
	}

	TEST_F(TypeDiscovererTest, RedefinitionOfType)
	{
		static const auto k_struct_name = "first_struct";

		// Prepare the types...
		auto first_struct_members = ASTNode::Dependencies{};
		first_struct_members.reserve(3);
		first_struct_members.emplace_back(VariableDeclarationNode::create("my_int", "i32", nullptr, false));
		first_struct_members.emplace_back(VariableDeclarationNode::create("my_float", "f64", nullptr, false));
		first_struct_members.emplace_back(VariableDeclarationNode::create("my_string", "str", nullptr, false));
		auto first_struct = StructDeclarationNode::create(k_struct_name, std::move(first_struct_members));

		auto second_struct_members = ASTNode::Dependencies{};
		second_struct_members.reserve(2);
		second_struct_members.emplace_back(VariableDeclarationNode::create("my_string", "str", nullptr, false));
		second_struct_members.emplace_back(VariableDeclarationNode::create("my_bool", "bool", nullptr, false));
		auto first_struct_redeclared = StructDeclarationNode::create(k_struct_name, std::move(second_struct_members));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(std::move(first_struct));
		module_statements.emplace_back(std::move(first_struct_redeclared));
		auto expected_module = ModuleNode::create("discovery_module", std::move(module_statements));

		// ...discover them...
		TypeDiscovererVisitor type_discoverer{};
		type_discoverer.accept(expected_module.get());

		// ...and verify the results.
		auto k_expected_types              = TypeDiscovererVisitor::basic_types();
		k_expected_types["first_struct"sv] = Type{ StructType{
			{ PrimitiveType::Kind::Int32, PrimitiveType::Kind::Float64, PrimitiveType::Kind::String } } };
		EXPECT_EQ(k_expected_types, type_discoverer.discovered_types());

		const auto& result_module = type_discoverer.cloned();
		ASSERT_TRUE(result_module->is<ModuleNode>());
		const auto& as_result_module = result_module->as<ModuleNode>();
		ASSERT_EQ(as_result_module.statements.size(), 2);

		ASSERT_TRUE(as_result_module.statements[0]->is<StructDeclarationNode>());
		const auto& as_struct_declaration = as_result_module.statements[0]->as<StructDeclarationNode>();
		ASSERT_EQ(as_struct_declaration.parameters.size(), 3);

		ASSERT_TRUE(as_struct_declaration.parameters[0]->is<VariableDeclarationNode>());
		const auto& first_parameter = as_struct_declaration.parameters[0]->as<VariableDeclarationNode>();
		EXPECT_EQ(first_parameter.name, "my_int");
		EXPECT_EQ(first_parameter.type_identifier, "i32");
		EXPECT_FALSE(first_parameter.expression);
		EXPECT_FALSE(first_parameter.is_mutable);

		ASSERT_TRUE(as_struct_declaration.parameters[1]->is<VariableDeclarationNode>());
		const auto& second_parameter = as_struct_declaration.parameters[1]->as<VariableDeclarationNode>();
		EXPECT_EQ(second_parameter.name, "my_float");
		EXPECT_EQ(second_parameter.type_identifier, "f64");
		EXPECT_FALSE(second_parameter.expression);
		EXPECT_FALSE(second_parameter.is_mutable);

		ASSERT_TRUE(as_struct_declaration.parameters[2]->is<VariableDeclarationNode>());
		const auto& third_parameter = as_struct_declaration.parameters[2]->as<VariableDeclarationNode>();
		EXPECT_EQ(third_parameter.name, "my_string");
		EXPECT_EQ(third_parameter.type_identifier, "str");
		EXPECT_FALSE(third_parameter.expression);
		EXPECT_FALSE(third_parameter.is_mutable);

		ASSERT_TRUE(as_result_module.statements[1]->is<ErrorNode>());
		const auto& as_error_node = as_result_module.statements[1]->as<ErrorNode>();
		EXPECT_EQ(as_error_node.message, std::format("redefinition of type '{}'", k_struct_name));
	}

	TEST_F(TypeDiscovererTest, TypeNotRegistered)
	{
		// Prepare the types...
		auto invalid_type_struct_members = ASTNode::Dependencies{};
		invalid_type_struct_members.reserve(3);
		invalid_type_struct_members.emplace_back(VariableDeclarationNode::create("my_int", "i32", nullptr, false));
		invalid_type_struct_members.emplace_back(
			VariableDeclarationNode::create("non_existing", "non_existing_type", nullptr, false));
		invalid_type_struct_members.emplace_back(VariableDeclarationNode::create("my_float", "f64", nullptr, false));
		auto invalid_type_struct = StructDeclarationNode::create("my_struct", std::move(invalid_type_struct_members));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(std::move(invalid_type_struct));
		auto expected_module = ModuleNode::create("discovery_module", std::move(module_statements));

		// ...discover them...
		TypeDiscovererVisitor type_discoverer{};
		type_discoverer.accept(expected_module.get());

		// ...and verify the results.
		const auto& result_module = type_discoverer.cloned();
		ASSERT_TRUE(result_module->is<ModuleNode>());
		const auto& as_result_module = result_module->as<ModuleNode>();
		ASSERT_EQ(as_result_module.statements.size(), 1);

		ASSERT_TRUE(as_result_module.statements[0]->is<StructDeclarationNode>());
		const auto& as_struct_declaration = as_result_module.statements[0]->as<StructDeclarationNode>();
		ASSERT_EQ(as_struct_declaration.parameters.size(), 3);

		ASSERT_TRUE(as_struct_declaration.parameters[0]->is<VariableDeclarationNode>());
		const auto& first_parameter = as_struct_declaration.parameters[0]->as<VariableDeclarationNode>();
		EXPECT_EQ(first_parameter.name, "my_int");
		EXPECT_EQ(first_parameter.type_identifier, "i32");
		EXPECT_FALSE(first_parameter.expression);
		EXPECT_FALSE(first_parameter.is_mutable);

		ASSERT_TRUE(as_struct_declaration.parameters[1]->is<ErrorNode>());
		const auto& second_parameter = as_struct_declaration.parameters[1]->as<ErrorNode>();
		EXPECT_EQ(second_parameter.message, "cannot resolve type 'non_existing_type', because no such type exists");

		ASSERT_TRUE(as_struct_declaration.parameters[2]->is<VariableDeclarationNode>());
		const auto& third_parameter = as_struct_declaration.parameters[2]->as<VariableDeclarationNode>();
		EXPECT_EQ(third_parameter.name, "my_float");
		EXPECT_EQ(third_parameter.type_identifier, "f64");
		EXPECT_FALSE(third_parameter.expression);
		EXPECT_FALSE(third_parameter.is_mutable);
	}
}  // namespace soul::ast::visitors::ut
