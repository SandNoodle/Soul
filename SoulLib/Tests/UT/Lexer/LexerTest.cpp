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
			Token(Token::Type::LiteralIdentifier, "my_identifier"sv, SourceOffset{ 1, 0 }),
			Token(Token::Type::LiteralIdentifier, "invalid_variable"sv, SourceOffset{ 1, 14 }),
			Token(Token::Type::LiteralIdentifier, "this_should_work"sv, SourceOffset{ 1, 32 }),
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
			Token(Token::Type::KeywordBreak, "break"sv, SourceOffset{ 1, 0 }),
			Token(Token::Type::KeywordCast, "cast"sv, SourceOffset{ 1, 6 }),
			Token(Token::Type::KeywordContinue, "continue"sv, SourceOffset{ 1, 11 }),
			Token(Token::Type::KeywordElse, "else"sv, SourceOffset{ 1, 20 }),
			Token(Token::Type::KeywordFalse, "false"sv, SourceOffset{ 1, 25 }),
			Token(Token::Type::KeywordFn, "fn"sv, SourceOffset{ 1, 31 }),
			Token(Token::Type::KeywordFor, "for"sv, SourceOffset{ 1, 34 }),
			Token(Token::Type::KeywordIf, "if"sv, SourceOffset{ 1, 38 }),
			Token(Token::Type::KeywordLet, "let"sv, SourceOffset{ 1, 41 }),
			Token(Token::Type::KeywordMut, "mut"sv, SourceOffset{ 1, 45 }),
			Token(Token::Type::KeywordNative, "native"sv, SourceOffset{ 1, 49 }),
			Token(Token::Type::KeywordReturn, "return"sv, SourceOffset{ 1, 56 }),
			Token(Token::Type::KeywordStruct, "struct"sv, SourceOffset{ 1, 63 }),
			Token(Token::Type::KeywordTrue, "true"sv, SourceOffset{ 1, 70 }),
			Token(Token::Type::KeywordWhile, "while"sv, SourceOffset{ 1, 75 }),
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
			Token(Token::Type::SymbolAmpersand, "&"sv, SourceOffset{ 1, 0 }),
			Token(Token::Type::SymbolAmpersandAmpersand, "&&"sv, SourceOffset{ 1, 2 }),
			Token(Token::Type::SymbolBang, "!"sv, SourceOffset{ 1, 5 }),
			Token(Token::Type::SymbolBangEqual, "!="sv, SourceOffset{ 1, 7 }),
			Token(Token::Type::SymbolBraceLeft, "{"sv, SourceOffset{ 1, 10 }),
			Token(Token::Type::SymbolBraceRight, "}"sv, SourceOffset{ 1, 12 }),
			Token(Token::Type::SymbolBracketLeft, "["sv, SourceOffset{ 1, 14 }),
			Token(Token::Type::SymbolBracketRight, "]"sv, SourceOffset{ 1, 16 }),
			Token(Token::Type::SymbolCaret, "^"sv, SourceOffset{ 1, 18 }),
			Token(Token::Type::SymbolColon, ":"sv, SourceOffset{ 1, 20 }),
			Token(Token::Type::SymbolColonColon, "::"sv, SourceOffset{ 1, 22 }),
			Token(Token::Type::SymbolComma, ","sv, SourceOffset{ 1, 25 }),
			Token(Token::Type::SymbolDot, "."sv, SourceOffset{ 1, 27 }),
			Token(Token::Type::SymbolEqual, "="sv, SourceOffset{ 1, 29 }),
			Token(Token::Type::SymbolEqualEqual, "=="sv, SourceOffset{ 1, 31 }),
			Token(Token::Type::SymbolGreater, ">"sv, SourceOffset{ 1, 34 }),
			Token(Token::Type::SymbolGreaterEqual, ">="sv, SourceOffset{ 1, 36 }),
			Token(Token::Type::SymbolLess, "<"sv, SourceOffset{ 1, 39 }),
			Token(Token::Type::SymbolLessEqual, "<="sv, SourceOffset{ 1, 41 }),
			Token(Token::Type::SymbolMinus, "-"sv, SourceOffset{ 1, 44 }),
			Token(Token::Type::SymbolMinusEqual, "-="sv, SourceOffset{ 1, 46 }),
			Token(Token::Type::SymbolMinusMinus, "--"sv, SourceOffset{ 1, 49 }),
			Token(Token::Type::SymbolPercent, "%"sv, SourceOffset{ 1, 52 }),
			Token(Token::Type::SymbolPercentEqual, "%="sv, SourceOffset{ 1, 54 }),
			Token(Token::Type::SymbolParenLeft, "("sv, SourceOffset{ 1, 57 }),
			Token(Token::Type::SymbolParenRight, ")"sv, SourceOffset{ 1, 59 }),
			Token(Token::Type::SymbolPipe, "|"sv, SourceOffset{ 1, 61 }),
			Token(Token::Type::SymbolPipePipe, "||"sv, SourceOffset{ 1, 63 }),
			Token(Token::Type::SymbolPlus, "+"sv, SourceOffset{ 1, 66 }),
			Token(Token::Type::SymbolPlusEqual, "+="sv, SourceOffset{ 1, 68 }),
			Token(Token::Type::SymbolPlusPlus, "++"sv, SourceOffset{ 1, 71 }),
			Token(Token::Type::SymbolQuestionMark, "?"sv, SourceOffset{ 1, 74 }),
			Token(Token::Type::SymbolSemicolon, ";"sv, SourceOffset{ 1, 76 }),
			Token(Token::Type::SymbolSlash, "/"sv, SourceOffset{ 1, 78 }),
			Token(Token::Type::SymbolSlashEqual, "/="sv, SourceOffset{ 1, 80 }),
			Token(Token::Type::SymbolStar, "*"sv, SourceOffset{ 1, 83 }),
			Token(Token::Type::SymbolStarEqual, "*="sv, SourceOffset{ 1, 85 }),
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
			Token(Token::Type::LiteralFloat, "0.0"sv, SourceOffset{ 1, 0 }),
			Token(Token::Type::LiteralFloat, "7.52"sv, SourceOffset{ 1, 4 }),
			Token(Token::Type::LiteralInteger, "4098"sv, SourceOffset{ 1, 9 }),
			Token(Token::Type::LiteralFloat, "4098.0"sv, SourceOffset{ 1, 14 }),
			Token(Token::Type::LiteralFloat, "-8192.32"sv, SourceOffset{ 1, 21 }),
			Token(Token::Type::LiteralFloat, "1000000000000.0"sv, SourceOffset{ 1, 30 }),
			Token(Token::Type::LiteralInteger, "0"sv, SourceOffset{ 1, 46 }),
			Token(Token::Type::LiteralInteger, "54"sv, SourceOffset{ 1, 48 }),
			Token(Token::Type::LiteralInteger, "1024"sv, SourceOffset{ 1, 51 }),
			Token(Token::Type::LiteralFloat, "-0.01"sv, SourceOffset{ 1, 56 }),
			Token(Token::Type::LiteralFloat, "5.47"sv, SourceOffset{ 1, 62 }),
			Token(Token::Type::LiteralInteger, "-8192"sv, SourceOffset{ 1, 67 }),
			Token(Token::Type::LiteralInteger, "1000000000000"sv, SourceOffset{ 1, 73 }),
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
			Token(Token::Type::LiteralString, "my_value"sv, SourceOffset{ 1, 2 }),
			Token(Token::Type::LiteralString, "no space after previous one"sv, SourceOffset{ 1, 12 }),
			Token(Token::Type::LiteralString, "520"sv, SourceOffset{ 1, 42 }),
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index], result_tokens[index]);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}

	TEST_F(LexerTest, Literals_Strings_UnterminatedString)
	{
		const std::string_view string = "\"this is an unterminated string, how sad :c";
		const auto result_tokens      = Lexer::Tokenize(string);

		ASSERT_EQ(result_tokens.size(), 1);
		EXPECT_EQ(
			result_tokens[0],
			Token(
				Token::Type::SpecialError, "unterminated string literal; did you forget '\"'?", SourceOffset{ 1, 0 }));
	}

	TEST_F(LexerTest, Compressed)
	{
		static constexpr auto k_input_string = "let variable:int=320;";
		const auto result_tokens             = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token(Token::Type::KeywordLet, "let"sv, SourceOffset{ 1, 0 }),
			Token(Token::Type::LiteralIdentifier, "variable"sv, SourceOffset{ 1, 4 }),
			Token(Token::Type::SymbolColon, ":"sv, SourceOffset{ 1, 12 }),
			Token(Token::Type::LiteralIdentifier, "int"sv, SourceOffset{ 1, 13 }),
			Token(Token::Type::SymbolEqual, "="sv, SourceOffset{ 1, 16 }),
			Token(Token::Type::LiteralInteger, "320"sv, SourceOffset{ 1, 17 }),
			Token(Token::Type::SymbolSemicolon, ";"sv, SourceOffset{ 1, 20 }),
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
			Token(Token::Type::KeywordFn, "fn"sv, SourceOffset{ 1, 0 }),
			Token(Token::Type::LiteralIdentifier, "main"sv, SourceOffset{ 1, 3 }),
			Token(Token::Type::SymbolParenLeft, "("sv, SourceOffset{ 1, 7 }),
			Token(Token::Type::LiteralIdentifier, "some_var"sv, SourceOffset{ 1, 8 }),
			Token(Token::Type::SymbolColon, ":"sv, SourceOffset{ 1, 17 }),
			Token(Token::Type::LiteralIdentifier, "int"sv, SourceOffset{ 1, 19 }),
			Token(Token::Type::SymbolParenRight, ")"sv, SourceOffset{ 1, 22 }),
			Token(Token::Type::SymbolColonColon, "::"sv, SourceOffset{ 1, 24 }),
			Token(Token::Type::LiteralIdentifier, "void"sv, SourceOffset{ 1, 27 }),
			Token(Token::Type::SymbolBraceLeft, "{"sv, SourceOffset{ 1, 32 }),
			Token(Token::Type::KeywordLet, "let"sv, SourceOffset{ 2, 1 }),
			Token(Token::Type::LiteralIdentifier, "my_variable"sv, SourceOffset{ 2, 5 }),
			Token(Token::Type::SymbolColon, ":"sv, SourceOffset{ 2, 17 }),
			Token(Token::Type::LiteralIdentifier, "str"sv, SourceOffset{ 2, 19 }),
			Token(Token::Type::SymbolEqual, "="sv, SourceOffset{ 2, 23 }),
			Token(Token::Type::LiteralString, "my_string"sv, SourceOffset{ 2, 27 }),
			Token(Token::Type::SymbolSemicolon, ";"sv, SourceOffset{ 2, 36 }),
			Token(Token::Type::KeywordReturn, "return"sv, SourceOffset{ 3, 1 }),
			Token(Token::Type::LiteralInteger, "0"sv, SourceOffset{ 3, 8 }),
			Token(Token::Type::SymbolSemicolon, ";"sv, SourceOffset{ 3, 9 }),
			Token(Token::Type::SymbolBraceRight, "}"sv, SourceOffset{ 4, 0 }),
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
			Token(Token::Type::SymbolSemicolon, ";"sv, SourceOffset{ 1, 1 }),
			Token(Token::Type::SymbolPlus, "+"sv, SourceOffset{ 3, 0 }),
			Token(Token::Type::SymbolPlusEqual, "+="sv, SourceOffset{ 4, 0 }),
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
			Token(Token::Type::LiteralIdentifier, "bool"sv, SourceOffset{ 1, 0 }),
			Token(Token::Type::LiteralIdentifier, "chr"sv, SourceOffset{ 1, 5 }),
			Token(Token::Type::LiteralIdentifier, "f32"sv, SourceOffset{ 1, 9 }),
			Token(Token::Type::LiteralIdentifier, "f64"sv, SourceOffset{ 1, 13 }),
			Token(Token::Type::LiteralIdentifier, "i32"sv, SourceOffset{ 1, 17 }),
			Token(Token::Type::LiteralIdentifier, "i64"sv, SourceOffset{ 1, 21 }),
			Token(Token::Type::LiteralIdentifier, "str"sv, SourceOffset{ 1, 25 }),
			Token(Token::Type::LiteralIdentifier, "void"sv, SourceOffset{ 1, 29 }),
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index], result_tokens[index]);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}
}  // namespace Soul::Lexer::UT
