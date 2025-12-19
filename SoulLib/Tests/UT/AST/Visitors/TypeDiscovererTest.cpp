#include "AST/Visitors/TypeDiscoverer.h"

#include <gtest/gtest.h>

#include "AST/AST.h"

#include <format>

namespace Soul::AST::Visitors::UT
{
	using namespace Soul::Types;
	using namespace Soul::Parser;
	using namespace std::string_view_literals;

	class TypeDiscovererTest : public ::testing::Test
	{
	};

	TEST_F(TypeDiscovererTest, StructUsedByOtherStruct)
	{
		// Prepare the types...
		auto first_struct_members = ASTNode::Dependencies{};
		first_struct_members.reserve(3);
		first_struct_members.emplace_back(
			VariableDeclarationNode::Create("my_int", k_base_specifier_i32, nullptr, false));
		first_struct_members.emplace_back(
			VariableDeclarationNode::Create("my_float", k_base_specifier_f64, nullptr, false));
		first_struct_members.emplace_back(
			VariableDeclarationNode::Create("my_string", k_base_specifier_str, nullptr, false));
		auto first_struct = StructDeclarationNode::Create("first_struct", std::move(first_struct_members));

		auto second_struct_members = ASTNode::Dependencies{};
		second_struct_members.reserve(2);
		second_struct_members.emplace_back(
			VariableDeclarationNode::Create("my_struct", BaseTypeSpecifier{ "first_struct"sv }, nullptr, false));
		second_struct_members.emplace_back(
			VariableDeclarationNode::Create("my_bool", k_base_specifier_bool, nullptr, false));
		auto second_struct = StructDeclarationNode::Create("second_struct", std::move(second_struct_members));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(std::move(first_struct));
		module_statements.emplace_back(std::move(second_struct));
		auto expected_module = ModuleNode::Create("discovery_module", std::move(module_statements));

		// ...discover them...
		TypeDiscovererVisitor type_discoverer{};
		type_discoverer.Accept(expected_module.get());

		// ...and verify the results.
		auto first_struct_type  = Type{ StructType{
			 { PrimitiveType::Kind::INT32, PrimitiveType::Kind::FLOAT64, PrimitiveType::Kind::STRING } } };
		auto second_struct_type = Type{ StructType{ { first_struct_type, PrimitiveType::Kind::BOOLEAN } } };

		auto k_expected_types = TypeDiscovererVisitor::GetBasicTypes();
		k_expected_types.emplace_back(std::make_pair(BaseTypeSpecifier{ "first_struct"sv }, first_struct_type));
		k_expected_types.emplace_back(std::make_pair(BaseTypeSpecifier{ "second_struct"sv }, second_struct_type));
		ASSERT_EQ(k_expected_types, type_discoverer.GetDiscoveredTypes());
	}

	TEST_F(TypeDiscovererTest, RedefinitionOfType)
	{
		static const auto k_base_specifier_struct_name = "first_struct";

		// Prepare the types...
		auto first_struct_members = ASTNode::Dependencies{};
		first_struct_members.reserve(3);
		first_struct_members.emplace_back(
			VariableDeclarationNode::Create("my_int", k_base_specifier_i32, nullptr, false));
		first_struct_members.emplace_back(
			VariableDeclarationNode::Create("my_float", k_base_specifier_f64, nullptr, false));
		first_struct_members.emplace_back(
			VariableDeclarationNode::Create("my_string", k_base_specifier_str, nullptr, false));
		auto first_struct
			= StructDeclarationNode::Create(k_base_specifier_struct_name, std::move(first_struct_members));

		auto second_struct_members = ASTNode::Dependencies{};
		second_struct_members.reserve(2);
		second_struct_members.emplace_back(
			VariableDeclarationNode::Create("my_string", k_base_specifier_str, nullptr, false));
		second_struct_members.emplace_back(
			VariableDeclarationNode::Create("my_bool", k_base_specifier_bool, nullptr, false));
		auto first_struct_redeclared
			= StructDeclarationNode::Create(k_base_specifier_struct_name, std::move(second_struct_members));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(std::move(first_struct));
		module_statements.emplace_back(std::move(first_struct_redeclared));
		auto expected_module = ModuleNode::Create("discovery_module", std::move(module_statements));

		// ...discover them...
		TypeDiscovererVisitor type_discoverer{};
		type_discoverer.Accept(expected_module.get());

		// ...and verify the results.
		auto k_expected_types = TypeDiscovererVisitor::GetBasicTypes();
		k_expected_types.emplace_back(std::make_pair(
			BaseTypeSpecifier{ "first_struct"sv },
			Type{ StructType{
				{ PrimitiveType::Kind::INT32, PrimitiveType::Kind::FLOAT64, PrimitiveType::Kind::STRING } } }));
		EXPECT_EQ(k_expected_types, type_discoverer.GetDiscoveredTypes());

		const auto& result_module = type_discoverer.Cloned();
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_result_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_result_module.statements.size(), 2);

		ASSERT_TRUE(as_result_module.statements[0]->Is<StructDeclarationNode>());
		const auto& as_struct_declaration = as_result_module.statements[0]->As<StructDeclarationNode>();
		ASSERT_EQ(as_struct_declaration.parameters.size(), 3);

		ASSERT_TRUE(as_struct_declaration.parameters[0]->Is<VariableDeclarationNode>());
		const auto& first_parameter = as_struct_declaration.parameters[0]->As<VariableDeclarationNode>();
		EXPECT_EQ(first_parameter.name, "my_int");
		EXPECT_EQ(first_parameter.type_specifier, k_base_specifier_i32);
		EXPECT_FALSE(first_parameter.expression);
		EXPECT_FALSE(first_parameter.is_mutable);

		ASSERT_TRUE(as_struct_declaration.parameters[1]->Is<VariableDeclarationNode>());
		const auto& second_parameter = as_struct_declaration.parameters[1]->As<VariableDeclarationNode>();
		EXPECT_EQ(second_parameter.name, "my_float");
		EXPECT_EQ(second_parameter.type_specifier, k_base_specifier_f64);
		EXPECT_FALSE(second_parameter.expression);
		EXPECT_FALSE(second_parameter.is_mutable);

		ASSERT_TRUE(as_struct_declaration.parameters[2]->Is<VariableDeclarationNode>());
		const auto& third_parameter = as_struct_declaration.parameters[2]->As<VariableDeclarationNode>();
		EXPECT_EQ(third_parameter.name, "my_string");
		EXPECT_EQ(third_parameter.type_specifier, k_base_specifier_str);
		EXPECT_FALSE(third_parameter.expression);
		EXPECT_FALSE(third_parameter.is_mutable);

		ASSERT_TRUE(as_result_module.statements[1]->Is<ErrorNode>());
		const auto& as_error_node = as_result_module.statements[1]->As<ErrorNode>();
		EXPECT_EQ(as_error_node.message, std::format("redefinition of type '{}'", k_base_specifier_struct_name));
	}

	TEST_F(TypeDiscovererTest, TypeNotRegistered)
	{
		// Prepare the types...
		auto invalid_type_struct_members = ASTNode::Dependencies{};
		invalid_type_struct_members.reserve(3);
		invalid_type_struct_members.emplace_back(
			VariableDeclarationNode::Create("my_int", k_base_specifier_i32, nullptr, false));
		invalid_type_struct_members.emplace_back(
			VariableDeclarationNode::Create("non_existing", BaseTypeSpecifier{ "non_existing_type" }, nullptr, false));
		invalid_type_struct_members.emplace_back(
			VariableDeclarationNode::Create("my_float", k_base_specifier_f64, nullptr, false));
		auto invalid_type_struct = StructDeclarationNode::Create("my_struct", std::move(invalid_type_struct_members));

		auto module_statements = ASTNode::Dependencies{};
		module_statements.emplace_back(std::move(invalid_type_struct));
		auto expected_module = ModuleNode::Create("discovery_module", std::move(module_statements));

		// ...discover them...
		TypeDiscovererVisitor type_discoverer{};
		type_discoverer.Accept(expected_module.get());

		// ...and verify the results.
		const auto& result_module = type_discoverer.Cloned();
		ASSERT_TRUE(result_module->Is<ModuleNode>());
		const auto& as_result_module = result_module->As<ModuleNode>();
		ASSERT_EQ(as_result_module.statements.size(), 1);

		ASSERT_TRUE(as_result_module.statements[0]->Is<StructDeclarationNode>());
		const auto& as_struct_declaration = as_result_module.statements[0]->As<StructDeclarationNode>();
		ASSERT_EQ(as_struct_declaration.parameters.size(), 3);

		ASSERT_TRUE(as_struct_declaration.parameters[0]->Is<VariableDeclarationNode>());
		const auto& first_parameter = as_struct_declaration.parameters[0]->As<VariableDeclarationNode>();
		EXPECT_EQ(first_parameter.name, "my_int");
		EXPECT_EQ(first_parameter.type_specifier, k_base_specifier_i32);
		EXPECT_FALSE(first_parameter.expression);
		EXPECT_FALSE(first_parameter.is_mutable);

		ASSERT_TRUE(as_struct_declaration.parameters[1]->Is<ErrorNode>());
		const auto& second_parameter = as_struct_declaration.parameters[1]->As<ErrorNode>();
		EXPECT_EQ(second_parameter.message, "cannot resolve type 'non_existing_type', because no such type exists");

		ASSERT_TRUE(as_struct_declaration.parameters[2]->Is<VariableDeclarationNode>());
		const auto& third_parameter = as_struct_declaration.parameters[2]->As<VariableDeclarationNode>();
		EXPECT_EQ(third_parameter.name, "my_float");
		EXPECT_EQ(third_parameter.type_specifier, k_base_specifier_f64);
		EXPECT_FALSE(third_parameter.expression);
		EXPECT_FALSE(third_parameter.is_mutable);
	}
}  // namespace Soul::AST::Visitors::UT
