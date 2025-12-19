#include <gtest/gtest.h>

#include "Lexer/Lexer.h"

#include <array>
#include <string_view>

namespace Soul::Lexer::UT
{
	using namespace std::string_view_literals;

	class LexerTest : public ::testing::Test
	{
	};

	TEST_F(LexerTest, EmptyString)
	{
		const auto result_tokens = Lexer::Tokenize(""sv);
		ASSERT_TRUE(result_tokens.empty());
	}

	TEST_F(LexerTest, Literals_Identifiers)
	{
		static constexpr auto k_input_string = "my_identifier invalid_variable this_should_work"sv;
		const auto result_tokens             = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token(Token::Type::LITERAL_IDENTIFIER, "my_identifier"sv, SourceOffset{ 1, 0 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "invalid_variable"sv, SourceOffset{ 1, 14 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "this_should_work"sv, SourceOffset{ 1, 32 }),
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index], result_tokens[index]);
			EXPECT_EQ(k_expected_tokens[index].location, k_expected_tokens[index].location);
		}
	}

	TEST_F(LexerTest, Literals_Keywords)
	{
		static constexpr auto k_input_string
			= "break cast continue else false fn for if let mut native return struct true while"sv;
		const auto result_tokens = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token(Token::Type::KEYWORD_BREAK, "break"sv, SourceOffset{ 1, 0 }),
			Token(Token::Type::KEYWORD_CAST, "cast"sv, SourceOffset{ 1, 6 }),
			Token(Token::Type::KEYWORD_CONTINUE, "continue"sv, SourceOffset{ 1, 11 }),
			Token(Token::Type::KEYWORD_ELSE, "else"sv, SourceOffset{ 1, 20 }),
			Token(Token::Type::KEYWORD_FALSE, "false"sv, SourceOffset{ 1, 25 }),
			Token(Token::Type::KEYWORD_FN, "fn"sv, SourceOffset{ 1, 31 }),
			Token(Token::Type::KEYWORD_FOR, "for"sv, SourceOffset{ 1, 34 }),
			Token(Token::Type::KEYWORD_IF, "if"sv, SourceOffset{ 1, 38 }),
			Token(Token::Type::KEYWORD_LET, "let"sv, SourceOffset{ 1, 41 }),
			Token(Token::Type::KEYWORD_MUT, "mut"sv, SourceOffset{ 1, 45 }),
			Token(Token::Type::KEYWORD_NATIVE, "native"sv, SourceOffset{ 1, 49 }),
			Token(Token::Type::KEYWORD_RETURN, "return"sv, SourceOffset{ 1, 56 }),
			Token(Token::Type::KEYWORD_STRUCT, "struct"sv, SourceOffset{ 1, 63 }),
			Token(Token::Type::KEYWORD_TRUE, "true"sv, SourceOffset{ 1, 70 }),
			Token(Token::Type::KEYWORD_WHILE, "while"sv, SourceOffset{ 1, 75 }),
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index], result_tokens[index]);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}

	TEST_F(LexerTest, SpecialCharacters)
	{
		static constexpr auto k_input_string
			= "& && ! != { } [ ] ^ : :: , . = == > >= < <= - -= -- % %= ( ) | || + += ++ ? ; / /= * *="sv;
		const auto result_tokens = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token(Token::Type::SYMBOL_AMPERSAND, "&"sv, SourceOffset{ 1, 0 }),
			Token(Token::Type::SYMBOL_AMPERSAND_AMPERSAND, "&&"sv, SourceOffset{ 1, 2 }),
			Token(Token::Type::SYMBOL_BANG, "!"sv, SourceOffset{ 1, 5 }),
			Token(Token::Type::SYMBOL_BANG_EQUAL, "!="sv, SourceOffset{ 1, 7 }),
			Token(Token::Type::SYMBOL_BRACE_LEFT, "{"sv, SourceOffset{ 1, 10 }),
			Token(Token::Type::SYMBOL_BRACE_RIGHT, "}"sv, SourceOffset{ 1, 12 }),
			Token(Token::Type::SYMBOL_BRACKET_LEFT, "["sv, SourceOffset{ 1, 14 }),
			Token(Token::Type::SYMBOL_BRACKET_RIGHT, "]"sv, SourceOffset{ 1, 16 }),
			Token(Token::Type::SYMBOL_CARET, "^"sv, SourceOffset{ 1, 18 }),
			Token(Token::Type::SYMBOL_COLON, ":"sv, SourceOffset{ 1, 20 }),
			Token(Token::Type::SYMBOL_COLON_COLON, "::"sv, SourceOffset{ 1, 22 }),
			Token(Token::Type::SYMBOL_COMMA, ","sv, SourceOffset{ 1, 25 }),
			Token(Token::Type::SYMBOL_DOT, "."sv, SourceOffset{ 1, 27 }),
			Token(Token::Type::SYMBOL_EQUAL, "="sv, SourceOffset{ 1, 29 }),
			Token(Token::Type::SYMBOL_EQUAL_EQUAL, "=="sv, SourceOffset{ 1, 31 }),
			Token(Token::Type::SYMBOL_GREATER, ">"sv, SourceOffset{ 1, 34 }),
			Token(Token::Type::SYMBOL_GREATER_EQUAL, ">="sv, SourceOffset{ 1, 36 }),
			Token(Token::Type::SYMBOL_LESS, "<"sv, SourceOffset{ 1, 39 }),
			Token(Token::Type::SYMBOL_LESS_EQUAL, "<="sv, SourceOffset{ 1, 41 }),
			Token(Token::Type::SYMBOL_MINUS, "-"sv, SourceOffset{ 1, 44 }),
			Token(Token::Type::SYMBOL_MINUS_EQUAL, "-="sv, SourceOffset{ 1, 46 }),
			Token(Token::Type::SYMBOL_MINUS_MINUS, "--"sv, SourceOffset{ 1, 49 }),
			Token(Token::Type::SYMBOL_PERCENT, "%"sv, SourceOffset{ 1, 52 }),
			Token(Token::Type::SYMBOL_PERCENT_EQUAL, "%="sv, SourceOffset{ 1, 54 }),
			Token(Token::Type::SYMBOL_PAREN_LEFT, "("sv, SourceOffset{ 1, 57 }),
			Token(Token::Type::SYMBOL_PAREN_RIGHT, ")"sv, SourceOffset{ 1, 59 }),
			Token(Token::Type::SYMBOL_PIPE, "|"sv, SourceOffset{ 1, 61 }),
			Token(Token::Type::SYMBOL_PIPE_PIPE, "||"sv, SourceOffset{ 1, 63 }),
			Token(Token::Type::SYMBOL_PLUS, "+"sv, SourceOffset{ 1, 66 }),
			Token(Token::Type::SYMBOL_PLUS_EQUAL, "+="sv, SourceOffset{ 1, 68 }),
			Token(Token::Type::SYMBOL_PLUS_PLUS, "++"sv, SourceOffset{ 1, 71 }),
			Token(Token::Type::SYMBOL_QUESTION_MARK, "?"sv, SourceOffset{ 1, 74 }),
			Token(Token::Type::SYMBOL_SEMICOLON, ";"sv, SourceOffset{ 1, 76 }),
			Token(Token::Type::SYMBOL_SLASH, "/"sv, SourceOffset{ 1, 78 }),
			Token(Token::Type::SYMBOL_SLASH_EQUAL, "/="sv, SourceOffset{ 1, 80 }),
			Token(Token::Type::SYMBOL_STAR, "*"sv, SourceOffset{ 1, 83 }),
			Token(Token::Type::SYMBOL_STAR_EQUAL, "*="sv, SourceOffset{ 1, 85 }),
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index], result_tokens[index]);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}

	TEST_F(LexerTest, Literals_Numbers)
	{
		static constexpr auto k_input_string
			= "0.0 7.52 4098 4098.0 -8192.32 1000000000000.0 0 54 1024 -0.01 5.47 -8192 1000000000000"sv;
		const auto result_tokens = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token(Token::Type::LITERAL_FLOAT, "0.0"sv, SourceOffset{ 1, 0 }),
			Token(Token::Type::LITERAL_FLOAT, "7.52"sv, SourceOffset{ 1, 4 }),
			Token(Token::Type::LITERAL_INTEGER, "4098"sv, SourceOffset{ 1, 9 }),
			Token(Token::Type::LITERAL_FLOAT, "4098.0"sv, SourceOffset{ 1, 14 }),
			Token(Token::Type::LITERAL_FLOAT, "-8192.32"sv, SourceOffset{ 1, 21 }),
			Token(Token::Type::LITERAL_FLOAT, "1000000000000.0"sv, SourceOffset{ 1, 30 }),
			Token(Token::Type::LITERAL_INTEGER, "0"sv, SourceOffset{ 1, 46 }),
			Token(Token::Type::LITERAL_INTEGER, "54"sv, SourceOffset{ 1, 48 }),
			Token(Token::Type::LITERAL_INTEGER, "1024"sv, SourceOffset{ 1, 51 }),
			Token(Token::Type::LITERAL_FLOAT, "-0.01"sv, SourceOffset{ 1, 56 }),
			Token(Token::Type::LITERAL_FLOAT, "5.47"sv, SourceOffset{ 1, 62 }),
			Token(Token::Type::LITERAL_INTEGER, "-8192"sv, SourceOffset{ 1, 67 }),
			Token(Token::Type::LITERAL_INTEGER, "1000000000000"sv, SourceOffset{ 1, 73 }),
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index], result_tokens[index]);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}

	TEST_F(LexerTest, Literals_Strings)
	{
		static constexpr auto k_input_string = "\"my_value\"\"no space after previous one\" \"520\""sv;
		const auto result_tokens             = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token(Token::Type::LITERAL_STRING, "my_value"sv, SourceOffset{ 1, 2 }),
			Token(Token::Type::LITERAL_STRING, "no space after previous one"sv, SourceOffset{ 1, 12 }),
			Token(Token::Type::LITERAL_STRING, "520"sv, SourceOffset{ 1, 42 }),
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index], result_tokens[index]);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}

	TEST_F(LexerTest, Literals_Strings_UnterminatedString)
	{
		static constexpr auto k_input_string = "\"this is an unterminated string, how sad :c";
		const auto result_tokens             = Lexer::Tokenize(k_input_string);

		ASSERT_EQ(result_tokens.size(), 1);
		EXPECT_EQ(
			result_tokens[0],
			Token(
				Token::Type::SPECIAL_ERROR, "unterminated string literal; did you forget '\"'?", SourceOffset{ 1, 0 }));
	}

	TEST_F(LexerTest, Compressed)
	{
		static constexpr auto k_input_string = "let variable:int=320;";
		const auto result_tokens             = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token(Token::Type::KEYWORD_LET, "let"sv, SourceOffset{ 1, 0 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "variable"sv, SourceOffset{ 1, 4 }),
			Token(Token::Type::SYMBOL_COLON, ":"sv, SourceOffset{ 1, 12 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "int"sv, SourceOffset{ 1, 13 }),
			Token(Token::Type::SYMBOL_EQUAL, "="sv, SourceOffset{ 1, 16 }),
			Token(Token::Type::LITERAL_INTEGER, "320"sv, SourceOffset{ 1, 17 }),
			Token(Token::Type::SYMBOL_SEMICOLON, ";"sv, SourceOffset{ 1, 20 }),
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index], result_tokens[index]);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}

	TEST_F(LexerTest, Mixed)
	{
		static constexpr auto k_input_string
			= "fn main(some_var : int) :: void { \n\tlet my_variable : str = \"my_string\";\n\treturn 0;\n} "sv;
		const auto result_tokens = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token(Token::Type::KEYWORD_FN, "fn"sv, SourceOffset{ 1, 0 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "main"sv, SourceOffset{ 1, 3 }),
			Token(Token::Type::SYMBOL_PAREN_LEFT, "("sv, SourceOffset{ 1, 7 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "some_var"sv, SourceOffset{ 1, 8 }),
			Token(Token::Type::SYMBOL_COLON, ":"sv, SourceOffset{ 1, 17 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "int"sv, SourceOffset{ 1, 19 }),
			Token(Token::Type::SYMBOL_PAREN_RIGHT, ")"sv, SourceOffset{ 1, 22 }),
			Token(Token::Type::SYMBOL_COLON_COLON, "::"sv, SourceOffset{ 1, 24 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "void"sv, SourceOffset{ 1, 27 }),
			Token(Token::Type::SYMBOL_BRACE_LEFT, "{"sv, SourceOffset{ 1, 32 }),
			Token(Token::Type::KEYWORD_LET, "let"sv, SourceOffset{ 2, 1 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "my_variable"sv, SourceOffset{ 2, 5 }),
			Token(Token::Type::SYMBOL_COLON, ":"sv, SourceOffset{ 2, 17 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "str"sv, SourceOffset{ 2, 19 }),
			Token(Token::Type::SYMBOL_EQUAL, "="sv, SourceOffset{ 2, 23 }),
			Token(Token::Type::LITERAL_STRING, "my_string"sv, SourceOffset{ 2, 27 }),
			Token(Token::Type::SYMBOL_SEMICOLON, ";"sv, SourceOffset{ 2, 36 }),
			Token(Token::Type::KEYWORD_RETURN, "return"sv, SourceOffset{ 3, 1 }),
			Token(Token::Type::LITERAL_INTEGER, "0"sv, SourceOffset{ 3, 8 }),
			Token(Token::Type::SYMBOL_SEMICOLON, ";"sv, SourceOffset{ 3, 9 }),
			Token(Token::Type::SYMBOL_BRACE_RIGHT, "}"sv, SourceOffset{ 4, 0 }),
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index], result_tokens[index]);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}

	TEST_F(LexerTest, WhitespacesAndComments)
	{
		static constexpr auto k_input_string = "\t\n\f     # this is a comment \n #and another one"sv;
		const auto result_tokens             = Lexer::Tokenize(k_input_string);
		ASSERT_TRUE(result_tokens.empty());
	}

	TEST_F(LexerTest, SymbolsDelimitedByComments)
	{
		static constexpr auto k_input_string = "\t;\n\f+# this is a comment \n+=#and another one"sv;
		const auto result_tokens             = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token(Token::Type::SYMBOL_SEMICOLON, ";"sv, SourceOffset{ 1, 1 }),
			Token(Token::Type::SYMBOL_PLUS, "+"sv, SourceOffset{ 3, 0 }),
			Token(Token::Type::SYMBOL_PLUS_EQUAL, "+="sv, SourceOffset{ 4, 0 }),
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index], result_tokens[index]);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}

	TEST_F(LexerTest, PrimitiveTypes)
	{
		static constexpr auto k_input_string = "bool chr f32 f64 i32 i64 str void"sv;
		const auto result_tokens             = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token(Token::Type::LITERAL_IDENTIFIER, "bool"sv, SourceOffset{ 1, 0 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "chr"sv, SourceOffset{ 1, 5 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "f32"sv, SourceOffset{ 1, 9 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "f64"sv, SourceOffset{ 1, 13 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "i32"sv, SourceOffset{ 1, 17 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "i64"sv, SourceOffset{ 1, 21 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "str"sv, SourceOffset{ 1, 25 }),
			Token(Token::Type::LITERAL_IDENTIFIER, "void"sv, SourceOffset{ 1, 29 }),
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index], result_tokens[index]);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}
}  // namespace Soul::Lexer::UT
