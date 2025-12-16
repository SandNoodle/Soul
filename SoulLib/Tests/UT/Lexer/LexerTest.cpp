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
			Token{ SourceOffset{ 1, 0 },  TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 14 }, TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 32 }, TokenType::TOKEN_LITERAL_IDENTIFIER },
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index].type, result_tokens[index].type);
			EXPECT_TRUE(SourceOffset::Equal(k_expected_tokens[index].location, k_expected_tokens[index].location));
		}
	}

	TEST_F(LexerTest, Literals_Keywords)
	{
		static constexpr auto k_input_string
			= "break cast continue else false fn for if let mut native return struct true while"sv;
		const auto result_tokens = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token{ SourceOffset{ 1, 0 },  TokenType::TOKEN_KEYWORD_BREAK    },
			Token{ SourceOffset{ 1, 6 },  TokenType::TOKEN_KEYWORD_CAST     },
			Token{ SourceOffset{ 1, 11 }, TokenType::TOKEN_KEYWORD_CONTINUE },
			Token{ SourceOffset{ 1, 20 }, TokenType::TOKEN_KEYWORD_ELSE     },
			Token{ SourceOffset{ 1, 25 }, TokenType::TOKEN_KEYWORD_FALSE    },
			Token{ SourceOffset{ 1, 31 }, TokenType::TOKEN_KEYWORD_FN       },
			Token{ SourceOffset{ 1, 34 }, TokenType::TOKEN_KEYWORD_FOR      },
			Token{ SourceOffset{ 1, 38 }, TokenType::TOKEN_KEYWORD_IF       },
			Token{ SourceOffset{ 1, 41 }, TokenType::TOKEN_KEYWORD_LET      },
			Token{ SourceOffset{ 1, 45 }, TokenType::TOKEN_KEYWORD_MUT      },
			Token{ SourceOffset{ 1, 49 }, TokenType::TOKEN_KEYWORD_NATIVE   },
			Token{ SourceOffset{ 1, 56 }, TokenType::TOKEN_KEYWORD_RETURN   },
			Token{ SourceOffset{ 1, 63 }, TokenType::TOKEN_KEYWORD_STRUCT   },
			Token{ SourceOffset{ 1, 70 }, TokenType::TOKEN_KEYWORD_TRUE     },
			Token{ SourceOffset{ 1, 75 }, TokenType::TOKEN_KEYWORD_WHILE    },
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index].type, result_tokens[index].type);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}

	TEST_F(LexerTest, SpecialCharacters)
	{
		static constexpr auto k_input_string
			= "& && ! != { } [ ] ^ : :: , . = == > >= < <= - -= -- % %= ( ) | || + += ++ ? ; / /= * *="sv;
		const auto result_tokens = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token{ SourceOffset{ 1, 0 },  TokenType::TOKEN_SYMBOL_AMPERSAND           },
			Token{ SourceOffset{ 1, 2 },  TokenType::TOKEN_SYMBOL_AMPERSAND_AMPERSAND },
			Token{ SourceOffset{ 1, 5 },  TokenType::TOKEN_SYMBOL_BANG                },
			Token{ SourceOffset{ 1, 7 },  TokenType::TOKEN_SYMBOL_BANG_EQUAL          },
			Token{ SourceOffset{ 1, 10 }, TokenType::TOKEN_SYMBOL_BRACE_LEFT          },
			Token{ SourceOffset{ 1, 12 }, TokenType::TOKEN_SYMBOL_BRACE_RIGHT         },
			Token{ SourceOffset{ 1, 14 }, TokenType::TOKEN_SYMBOL_BRACKET_LEFT        },
			Token{ SourceOffset{ 1, 16 }, TokenType::TOKEN_SYMBOL_BRACKET_RIGHT       },
			Token{ SourceOffset{ 1, 18 }, TokenType::TOKEN_SYMBOL_CARET               },
			Token{ SourceOffset{ 1, 20 }, TokenType::TOKEN_SYMBOL_COLON               },
			Token{ SourceOffset{ 1, 22 }, TokenType::TOKEN_SYMBOL_COLON_COLON         },
			Token{ SourceOffset{ 1, 25 }, TokenType::TOKEN_SYMBOL_COMMA               },
			Token{ SourceOffset{ 1, 27 }, TokenType::TOKEN_SYMBOL_DOT                 },
			Token{ SourceOffset{ 1, 29 }, TokenType::TOKEN_SYMBOL_EQUAL               },
			Token{ SourceOffset{ 1, 31 }, TokenType::TOKEN_SYMBOL_EQUAL_EQUAL         },
			Token{ SourceOffset{ 1, 34 }, TokenType::TOKEN_SYMBOL_GREATER             },
			Token{ SourceOffset{ 1, 36 }, TokenType::TOKEN_SYMBOL_GREATER_EQUAL       },
			Token{ SourceOffset{ 1, 39 }, TokenType::TOKEN_SYMBOL_LESS                },
			Token{ SourceOffset{ 1, 41 }, TokenType::TOKEN_SYMBOL_LESS_EQUAL          },
			Token{ SourceOffset{ 1, 44 }, TokenType::TOKEN_SYMBOL_MINUS               },
			Token{ SourceOffset{ 1, 46 }, TokenType::TOKEN_SYMBOL_MINUS_EQUAL         },
			Token{ SourceOffset{ 1, 49 }, TokenType::TOKEN_SYMBOL_MINUS_MINUS         },
			Token{ SourceOffset{ 1, 52 }, TokenType::TOKEN_SYMBOL_PERCENT             },
			Token{ SourceOffset{ 1, 54 }, TokenType::TOKEN_SYMBOL_PERCENT_EQUAL       },
			Token{ SourceOffset{ 1, 57 }, TokenType::TOKEN_SYMBOL_PAREN_LEFT          },
			Token{ SourceOffset{ 1, 59 }, TokenType::TOKEN_SYMBOL_PAREN_RIGHT         },
			Token{ SourceOffset{ 1, 61 }, TokenType::TOKEN_SYMBOL_PIPE                },
			Token{ SourceOffset{ 1, 63 }, TokenType::TOKEN_SYMBOL_PIPE_PIPE           },
			Token{ SourceOffset{ 1, 66 }, TokenType::TOKEN_SYMBOL_PLUS                },
			Token{ SourceOffset{ 1, 68 }, TokenType::TOKEN_SYMBOL_PLUS_EQUAL          },
			Token{ SourceOffset{ 1, 71 }, TokenType::TOKEN_SYMBOL_PLUS_PLUS           },
			Token{ SourceOffset{ 1, 74 }, TokenType::TOKEN_SYMBOL_QUESTION_MARK       },
			Token{ SourceOffset{ 1, 76 }, TokenType::TOKEN_SYMBOL_SEMICOLON           },
			Token{ SourceOffset{ 1, 78 }, TokenType::TOKEN_SYMBOL_SLASH               },
			Token{ SourceOffset{ 1, 80 }, TokenType::TOKEN_SYMBOL_SLASH_EQUAL         },
			Token{ SourceOffset{ 1, 83 }, TokenType::TOKEN_SYMBOL_STAR                },
			Token{ SourceOffset{ 1, 85 }, TokenType::TOKEN_SYMBOL_STAR_EQUAL          },
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index].type, result_tokens[index].type);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}

	TEST_F(LexerTest, Literals_Numbers)
	{
		static constexpr auto k_input_string
			= "0.0 7.52 4098 4098.0 -8192.32 1000000000000.0 0 54 1024 -0.01 5.47 -8192 1000000000000"sv;
		const auto result_tokens = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token{ SourceOffset{ 1, 0 },  TokenType::TOKEN_LITERAL_FLOAT   },
			Token{ SourceOffset{ 1, 4 },  TokenType::TOKEN_LITERAL_FLOAT   },
			Token{ SourceOffset{ 1, 9 },  TokenType::TOKEN_LITERAL_INTEGER },
			Token{ SourceOffset{ 1, 14 }, TokenType::TOKEN_LITERAL_FLOAT   },
			Token{ SourceOffset{ 1, 21 }, TokenType::TOKEN_LITERAL_FLOAT   },
			Token{ SourceOffset{ 1, 30 }, TokenType::TOKEN_LITERAL_FLOAT   },
			Token{ SourceOffset{ 1, 46 }, TokenType::TOKEN_LITERAL_INTEGER },
			Token{ SourceOffset{ 1, 48 }, TokenType::TOKEN_LITERAL_INTEGER },
			Token{ SourceOffset{ 1, 51 }, TokenType::TOKEN_LITERAL_INTEGER },
			Token{ SourceOffset{ 1, 56 }, TokenType::TOKEN_LITERAL_FLOAT   },
			Token{ SourceOffset{ 1, 62 }, TokenType::TOKEN_LITERAL_FLOAT   },
			Token{ SourceOffset{ 1, 67 }, TokenType::TOKEN_LITERAL_INTEGER },
			Token{ SourceOffset{ 1, 73 }, TokenType::TOKEN_LITERAL_INTEGER },
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index].type, result_tokens[index].type);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}

	TEST_F(LexerTest, Literals_Strings)
	{
		static constexpr auto k_input_string = "\"my_value\"\"no space after previous one\" \"520\""sv;
		const auto result_tokens             = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token{ SourceOffset{ 1, 2 },  TokenType::TOKEN_LITERAL_STRING },
			Token{ SourceOffset{ 1, 12 }, TokenType::TOKEN_LITERAL_STRING },
			Token{ SourceOffset{ 1, 42 }, TokenType::TOKEN_LITERAL_STRING },
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index].type, result_tokens[index].type);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}

	TEST_F(LexerTest, Literals_Strings_UnterminatedString)
	{
		static constexpr std::string_view k_input_string = "\"this is an unterminated string, how sad :c";
		const auto result_tokens                         = Lexer::Tokenize(k_input_string);

		ASSERT_EQ(result_tokens.size(), 1);
		EXPECT_EQ(result_tokens[0].type, TokenType::TOKEN_SPECIAL_ERROR);
		EXPECT_EQ(result_tokens[0].location, SourceOffset(1, 0));
	}

	TEST_F(LexerTest, Compressed)
	{
		static constexpr auto k_input_string = "let variable:int=320;";
		const auto result_tokens             = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token{ SourceOffset{ 1, 0 },  TokenType::TOKEN_KEYWORD_LET        },
			Token{ SourceOffset{ 1, 4 },  TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 12 }, TokenType::TOKEN_SYMBOL_COLON       },
			Token{ SourceOffset{ 1, 13 }, TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 16 }, TokenType::TOKEN_SYMBOL_EQUAL       },
			Token{ SourceOffset{ 1, 17 }, TokenType::TOKEN_LITERAL_INTEGER    },
			Token{ SourceOffset{ 1, 20 }, TokenType::TOKEN_SYMBOL_SEMICOLON   },
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index].type, result_tokens[index].type);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}

	TEST_F(LexerTest, Mixed)
	{
		static constexpr auto k_input_string
			= "fn main(some_var : int) :: void { \n\tlet my_variable : str = \"my_string\";\n\treturn 0;\n} "sv;
		const auto result_tokens = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token{ SourceOffset{ 1, 0 },  TokenType::TOKEN_KEYWORD_FN         },
			Token{ SourceOffset{ 1, 3 },  TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 7 },  TokenType::TOKEN_SYMBOL_PAREN_LEFT  },
			Token{ SourceOffset{ 1, 8 },  TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 17 }, TokenType::TOKEN_SYMBOL_COLON       },
			Token{ SourceOffset{ 1, 19 }, TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 22 }, TokenType::TOKEN_SYMBOL_PAREN_RIGHT },
			Token{ SourceOffset{ 1, 24 }, TokenType::TOKEN_SYMBOL_COLON_COLON },
			Token{ SourceOffset{ 1, 27 }, TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 32 }, TokenType::TOKEN_SYMBOL_BRACE_LEFT  },
			Token{ SourceOffset{ 2, 1 },  TokenType::TOKEN_KEYWORD_LET        },
			Token{ SourceOffset{ 2, 5 },  TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 2, 17 }, TokenType::TOKEN_SYMBOL_COLON       },
			Token{ SourceOffset{ 2, 19 }, TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 2, 23 }, TokenType::TOKEN_SYMBOL_EQUAL       },
			Token{ SourceOffset{ 2, 27 }, TokenType::TOKEN_LITERAL_STRING     },
			Token{ SourceOffset{ 2, 36 }, TokenType::TOKEN_SYMBOL_SEMICOLON   },
			Token{ SourceOffset{ 3, 1 },  TokenType::TOKEN_KEYWORD_RETURN     },
			Token{ SourceOffset{ 3, 8 },  TokenType::TOKEN_LITERAL_INTEGER    },
			Token{ SourceOffset{ 3, 9 },  TokenType::TOKEN_SYMBOL_SEMICOLON   },
			Token{ SourceOffset{ 4, 0 },  TokenType::TOKEN_SYMBOL_BRACE_RIGHT },
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index].type, result_tokens[index].type);
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
			Token{ SourceOffset{ 1, 1 }, TokenType::TOKEN_SYMBOL_SEMICOLON  },
			Token{ SourceOffset{ 3, 0 }, TokenType::TOKEN_SYMBOL_PLUS       },
			Token{ SourceOffset{ 4, 0 }, TokenType::TOKEN_SYMBOL_PLUS_EQUAL },
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index].type, result_tokens[index].type);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}

	TEST_F(LexerTest, PrimitiveTypes)
	{
		static constexpr auto k_input_string = "bool chr f32 f64 i32 i64 str void"sv;
		const auto result_tokens             = Lexer::Tokenize(k_input_string);

		static constexpr std::array k_expected_tokens = {
			Token{ SourceOffset{ 1, 0 },  TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 5 },  TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 9 },  TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 13 }, TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 17 }, TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 21 }, TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 25 }, TokenType::TOKEN_LITERAL_IDENTIFIER },
			Token{ SourceOffset{ 1, 29 }, TokenType::TOKEN_LITERAL_IDENTIFIER },
		};

		ASSERT_EQ(k_expected_tokens.size(), result_tokens.size());
		for (size_t index = 0; index < k_expected_tokens.size(); ++index) {
			EXPECT_EQ(k_expected_tokens[index].type, result_tokens[index].type);
			EXPECT_EQ(k_expected_tokens[index].location, result_tokens[index].location);
		}
	}
}  // namespace Soul::Lexer::UT
