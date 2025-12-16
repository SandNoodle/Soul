#include <gtest/gtest.h>

#include "ast/visitors/stringify.h"
#include "lexer/Lexer.h"
#include "parser/parser.h"

#include <filesystem>
#include <fstream>
#include <optional>
#include <set>
#include <source_location>

namespace soul::parser::ut
{
	using namespace soul::ast::visitors;
	using namespace soul::lexer;
	using namespace soul::parser;

	struct Case
	{
		std::string name;
		std::filesystem::path script_path;
		std::filesystem::path expected_output_path;
	};

	class ParserTest : public ::testing::TestWithParam<Case>
	{
		protected:
		std::optional<std::string> read_file(const std::filesystem::path& path)
		try {
			static constexpr auto k_read_size = std::size_t{ 4096 };

			std::ifstream stream(path, std::ios::in | std::ios::binary);
			stream.exceptions(std::ios::badbit);
			if (stream.fail()) {
				return std::nullopt;
			}

			// Read the data.
			std::string data;  // @TODO: Reserve size.
			std::string buffer(k_read_size, '\0');
			while (stream.read(&buffer[0], k_read_size)) {
				data.append(buffer, 0, stream.gcount());
			}
			data.append(buffer, 0, stream.gcount());
			stream.close();

			return data;
		} catch (std::exception& e) {
			return std::nullopt;
		}

		public:
		static std::vector<Case> generate_cases()
		{
			static const auto k_base_directory
				= std::filesystem::path{ std::source_location::current().file_name() }.parent_path() / "cases";
			std::set<std::filesystem::path> unique_files;
			for (const auto& entry : std::filesystem::recursive_directory_iterator(k_base_directory)) {
				if (entry.is_directory()) {
					continue;
				}
				unique_files.insert(entry.path().parent_path() / entry.path().stem());
			}

			std::vector<Case> cases;
			cases.reserve(unique_files.size());
			for (const auto& entry : unique_files) {
				cases.emplace_back(Case{
					.name                 = entry.stem().string(),
					.script_path          = std::filesystem::path{ entry }.replace_extension(".soul"),
					.expected_output_path = std::filesystem::path{ entry }.replace_extension(".json"),
				});
			}

			return cases;
		}
	};

	INSTANTIATE_TEST_SUITE_P(ParserTest,
	                         ParserTest,
	                         ::testing::ValuesIn(ParserTest::generate_cases()),
	                         [](const ::testing::TestParamInfo<Case>& info) {
								 const auto& param = info.param;
								 return param.name;
							 });

	TEST_P(ParserTest, AllCases)
	{
		const auto& param = GetParam();

		const auto input = read_file(param.script_path);
		ASSERT_TRUE(input.has_value()) << "failed to read: " << param.script_path;

		const auto tokens      = Lexer::Tokenize(*input);  // NOLINT(bugprone-unchecked-optional-access)
		const auto result_tree = Parser::parse("test_module", tokens);

		StringifyVisitor stringify;
		stringify.accept(result_tree.get());

		static constexpr bool k_regenerate_cases = false;
		if constexpr (k_regenerate_cases) try {
				std::filesystem::remove(param.expected_output_path);
				std::ofstream file(param.expected_output_path, std::ios::binary);
				file << stringify.string();
				file.close();
				GTEST_FAIL() << "Please turn off the `k_regenerate_cases` flag.";
			} catch (const std::exception& e) {
				GTEST_FAIL() << "Failed to regenerate cases, because: " << e.what();
			}

		const auto expected_output = read_file(param.expected_output_path);
		ASSERT_TRUE(expected_output.has_value()) << "failed to read: " << param.expected_output_path;
		ASSERT_EQ(expected_output.value(), stringify.string());  // NOLINT(bugprone-unchecked-optional-access)
	}
}  // namespace soul::parser::ut
