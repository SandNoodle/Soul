#include "lexer/lexer.h"

#include <gtest/gtest.h>

#include <string.h>

// TODO: Think about helper function that would return an array of strings
//       from a given string, basically splitting words after each whitespace.
//       This way we don't have to maintain them manually.
//       Signature:
//          soul_string_array_t split_string(const char* str, size_t length);
namespace
{
	#define ARRAY_SIZE(x) sizeof((x)) / sizeof((x)[0])

	class LexerTest : public ::testing::Test
	{
		public:
			soul_lexer_t lexer;
			soul_token_array_t array;

			void SetUp() override
			{
				lexer = soul_lexer_create();
			}

			void TearDown() override
			{
				soul_token_array_destroy(&array);
			}

			soul_token_array_t foo(const char* str)
			{
				const size_t str_size = strlen(str);
				return soul_lexer_scan(&lexer, str, str_size);
			}
	};

	TEST_F(LexerTest, EmptyString)
	{
		const char* str = "";
		result_array = foo(str);

		ASSERT_EQ(result_array.tokens, NULL);
		ASSERT_EQ(result_array.size, 0);
		ASSERT_EQ(result_array.capacity, 0);
	}

	TEST_F(LexerTest, EndOfFile)
	{
		const char* str = "\0";
		result_array = foo(str);

		ASSERT_NE(result_array.tokens, NULL);
		ASSERT_EQ(result_array.size, 1);
		ASSERT_EQ(result_array[0].type, soul_token_eof);
	}

	TEST_F(LexerTest, Comments)
	{
		const char* str = "# This is a comment and should be ignored.";
		result_array = foo(str);

		ASSERT_EQ(result_array.tokens, NULL);
		ASSERT_EQ(result_array.size, 0);
		ASSERT_EQ(result_array.capacity, 0);
	}

	TEST_F(LexerTest, ErrorToken)
	{
		// This string below is not a valid token in this language.
		const char* str = "$";
		result_array = foo(str);
		ASSERT_EQ(result_array.size, 1);

		const char* error_message = "unrecognized token";
		const soul_token_t actual_token = result_array.tokens[0];
		ASSERT_EQ(actual_token.type, soul_token_error);
		ASSERT_EQ(actual_token.length, strlen(error_message));
		ASSERT_EQ(actual_token.start, error_message);
	}

	TEST_F(LexerTest, Keywords)
	{
		// String has to match with order the keywords specified in token.h
		const char* str = "native import define fn let mut if else for while continue break struct enum true false";
		result_array = foo(str);

		soul_token_type_t expected_types[soul_token_keywords_size] = {
			soul_token_native,
			soul_token_import,
			soul_token_define,
			soul_token_let, soul_token_mut,
			soul_token_if, soul_token_else,
			soul_token_for, soul_token_while,
			soul_token_continue, soul_token_break,
			soul_token_return, soul_token_fn,
			soul_token_struct, soul_token_enum,
			soul_token_true, soul_token_false,
		};
		ASSERT_EQ(result_array.size, ARRAY_SIZE(expected_types));

		for(size_t i = 0; i < result_array.size; ++i)
		{ 
			const soul_token_type_t actual_type = soul_token_array_type_at(&result_array.tokens, i);
			const soul_token_type_t expected_type = expected_types[i];
			ASSERT_EQ(actual_type, expected_type);
		}
	}

	TEST_F(LexerTest, Literals_Numbers)
	{
		const size_t expected_count = 8;
		const char* str = "52000 9846435 4.7 0 -0.1 6 -20 -9840.8";
		result_array = foo(str);

		const char* expected_strings[expected_count] = {
			"52000", "9846435", "4.7", "0", "-0.1", "6", "-20", "-9840.8";
		}
		ASSERT_EQ(result_array.size, ARRAY_SIZE(expected_strings));

		for(size_t i = 0; i < expected_count; ++i)
		{
			const soul_token_t acutal_token = soul_token_array_at(&result_array, i);
			ASSERT_TRUE(actual_token.type, soul_token_number);

			const size_t expected_length = strlen(expected_strings[i]);
			ASSERT_EQ(expected_length, actual_token.length);
			ASSERT_EQ(expected_strings[i], actual_token.start);
		}
	}

	TEST_F(LexerTest, Literals_Strings)
	{
		const size_t expected_count = 2;
		const char* str = "\"test string\" \"\"";
		result_array = foo(str);

		const char* expected_strings[expected_count] = {
			"\"test string\"", ""
		};
		ASSERT_EQ(result_array.size, ARRAY_SIZE(expected_strings));

		for (size_t i = 0; i < expected_count; ++i)
		{
			const soul_token_t acutal_token = soul_token_array_at(&result_array, i);
			ASSERT_TRUE(actual_token.type, soul_token_string);

			const size_t expected_length = strlen(expected_strings[i]);
			ASSERT_EQ(expected_length, actual_token.length);
			ASSERT_EQ(expected_strings[i], actual_token.start);
		}
	}

	TEST_F(LexerTest, Literals_Booleans)
	{
		const size_t expected_count = 2;
		const char* str = "true false";
		result_array = foo(str);

		const char* expected_strings[expected_count] = {
			"true", "false"
		};
		ASSERT_EQ(result_array.size, ARRAY_SIZE(expected_strings));

		for (size_t i = 0; i < expected_count; ++i)
		{
			const soul_token_t acutal_token = soul_token_array_at(&result_array, i);
			ASSERT_TRUE(actual_token.type, soul_token_bool);

			const size_t expected_length = strlen(expected_strings[i]);
			ASSERT_EQ(expected_length, actual_token.length);
			ASSERT_EQ(expected_strings[i], actual_token.start);
		}
	}

	TEST_F(LexerTest, Literals_Mix)
	{
		const char* str = "\"test string\" true 5.42 -3.14 false 0 \"true\"";
		result_array = foo(str);

		const char* expected_strings[expected_count] = {
			"\"test string\"", "true", "5.42", "-3.14", "false", "0", "\"true\"";
		};
		ASSERT_EQ(result_array.size, ARRAY_SIZE(expected_strings));

		for (size_t i = 0; i < expected_count; ++i)
		{
			const soul_token_t acutal_token = soul_token_array_at(&result_array, i);
			ASSERT_TRUE(actual_token.type, soul_token_bool);

			const size_t expected_length = strlen(expected_strings[i]);
			ASSERT_EQ(expected_length, actual_token.length);
			ASSERT_EQ(expected_strings[i], actual_token.start);
		}
	}

	TEST_F(LexerTest, Characters)
	{
		const size_t expected_count = 33;
		const char* str = ": :: = == ! != > >= < <= + += ++ - -= -- * *= / /= & && | ||";
		result_array = foo(str);
		ASSERT_NE(result_array.tokens, NULL);
		ASSERT_EQ(result_array.size, expected_count);

		const char* expected_strings[expected_count] = {
			":", "::", "=", "==", "!", "!=", ">", ">=",
			"<", "<=", "+", "+=", "++", "-", "-=", "--",
			"*", "*=", "/", "/=", "&", "&&", "|", "||";
		};
		ASSERT_EQ(result_array.size, ARRAY_SIZE(expected_strings));

		const soul_token_type_t expected_types = {
			soul_token_colon, soul_token_double_colon,
			soul_token_equal, soul_token_double_equal,
			soul_token_bang, soul_token_bang_equal,
			soul_token_greater, soul_token_greater_equal,
			soul_token_less, soul_token_less_equal,
			soul_token_plus, soul_token_plus_equal, soul_token_double_plus,
			soul_token_minus, soul_token_minus_equal, soul_token_double_minus,
			soul_token_star, soul_token_star_equal,
			soul_token_slash, soul_token_slash_equal,
			soul_token_ampersand, soul_token_double_ampersand,
			soul_token_pipe, soul_token_double_pipe,
		};

		ASSERT_EQ(ARRAY_SIZE(expected_types), ARRAY_SIZE(expected_strings));

		for (size_t i = 0; i < expected_count; ++i)
		{
			const soul_token_t acutal_token = soul_token_array_at(&result_array, i);
			ASSERT_TRUE(actual_token.type, expected_types[i]);

			const size_t expected_length = strlen(expected_strings[i]);
			ASSERT_EQ(expected_length, actual_token.length);
			ASSERT_EQ(expected_strings[i], actual_token.start);
		}
	}
}

