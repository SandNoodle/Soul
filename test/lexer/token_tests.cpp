#include "lexer/token.h"

#include <gtest/gtest.h>

namespace 
{
	class TokenArrayTest : public ::testing::Test {
		public:
			soul_token_array_t array

			void SetUp() override
			{
				array = soul_token_array_create();
			}

			void TearDown() override
			{
				soul_token_array_destroy(&array);
			}
	};

	TEST_F(TokenArrayTest, CreateAndDestroy)
	{
		array = soul_token_array_create();
		ASSERT_EQ(array.tokens, NULL);
		ASSERT_EQ(array.size, 0);
		ASSERT_EQ(array.capacity, 0);

		soul_token_t token = {
			.type = soul_token_unknown,
			.start = NULL,
			.length = 0,
		};
		soul_token_array_append(&array, token);
		ASSERT_NE(array.tokens, NULL);
		ASSERT_EQ(array.size, 1);
		ASSERT_EQ(array.capacity, 8);
		ASSERT_EQ(array.tokens[0], token);

		soul_token_array_destroy(&array);
		ASSERT_EQ(array.tokens, NULL);
		ASSERT_EQ(array.size, 0);
		ASSERT_EQ(array.capacity, 0);
	}

	TEST_F(TokenArrayTest, CreateAndDestroy_Empty)
	{
		array = soul_token_array_create();
		ASSERT_EQ(array.tokens, NULL);
		ASSERT_EQ(array.size, 0);
		ASSERT_EQ(array.capacity, 0);

		soul_token_array_destroy(&array);
		ASSERT_EQ(array.tokens, NULL);
		ASSERT_EQ(array.size, 0);
		ASSERT_EQ(array.capacity, 0);
	}

	TEST_F(TokenArrayTest, Append)
	{
		array = soul_token_array_create();
		ASSERT_EQ(array.tokens, NULL);
		ASSERT_EQ(array.size, 0);
		ASSERT_EQ(array.capacity, 0);

		soul_token_t token = {
			.type = soul_token_error,
			.start = NULL,
			.length = 0,
		};

		const size_t capacity = SOUL_ARRAY_MIN_CAPACITY;
		for(size_t i = 0; i < capacity; ++i)
		{
			soul_token_array_append(&array, token);
			ASSERT_NE(array.tokens, NULL);
			ASSERT_EQ(array.size, i + 1);
			ASSERT_EQ(array.capacity, capacity);
			ASSERT_EQ(array.tokens[i], token);
		}

		soul_token_array_destroy(&array);
		ASSERT_EQ(array.tokens, NULL);
		ASSERT_EQ(array.size, 0);
		ASSERT_EQ(array.capacity, 0);
	}

	TEST_F(TokenArrayTest, TokenAt_ValidIndex)
	{
		array = soul_token_array_create();

		const char* message = "some message";
		soul_token_t expected_token = {
			.type = soul_token_unknown,
			.start = message,
			.length = strlen(message),
		};
		soul_token_array_append(&array, expected_token);
		ASSERT_NE(array.tokens, NULL);
		ASSERT_EQ(array.size, 1);

		soul_token_t actual_token = soul_token_array_at(&array, 0);
		ASSERT_EQ(expected_token.type, actual_token.type);
		ASSERT_EQ(expected_token.length, actual_token.length);
		ASSERT_EQ(expected_token.start, actual_token.start);

		soul_token_array_destroy(&array);
	}

	TEST_F(TokenArrayTest, TokenAt_InvalidIndex)
	{
		array = soul_token_array_create();

		const char* error_message = "index out of range";
		soul_token_t expected_token = {
			.type = soul_token_error,
			.start = error_message,
			.length = strlen(error_message),
		};

		soul_token_t actual_token = soul_token_array_at(&array, 0);
		ASSERT_EQ(expected_token.type, actual_token.type);
		ASSERT_EQ(expected_token.length, actual_token.length);
		ASSERT_EQ(expected_token.start, actual_token.start);

		soul_token_array_destroy(&array);
	}

	TEST_F(TokenArrayTest, TypeAt_ValidIndex)
	{
		array = soul_token_array_create();

		const char* message = "some message";
		soul_token_t expected_token = {
			.type = soul_token_unknown,
			.start = message,
			.length = strlen(message),
		};
		soul_token_array_append(&array, expected_token);
		ASSERT_NE(array.tokens, NULL);
		ASSERT_EQ(array.size, 1);

		soul_token_t actual_token = soul_token_array_type_at(&array, 0);
		ASSERT_EQ(expected_token.type, actual_token.type);

		soul_token_array_destroy(&array);
	}

	TEST_F(TokenArrayTest, TypeAt_InvalidIndex)
	{
		array = soul_token_array_create();

		soul_token_t expected_token = {
			.type = soul_token_error,
			.start = NULL,
			.length = 0,
		};
		soul_token_array_append(&array, expected_token);
		ASSERT_NE(array.tokens, NULL);
		ASSERT_EQ(array.size, 1);

		soul_token_t actual_token = soul_token_array_type_at(&array, 0);
		ASSERT_EQ(expected_token.type, actual_token.type);

		soul_token_array_destroy(&array);
	}

	TEST_F(TokenArrayTest, TypeBack)
	{
		array = soul_token_array_create();

		const char* message = "some message";
		soul_token_t expected_token = {
			.type = soul_token_unknown,
			.start = message,
			.length = strlen(message),
		};
		soul_token_array_append(&array, expected_token);
		ASSERT_NE(array.tokens, NULL);
		ASSERT_EQ(array.size, 1);

		soul_token_t actual_token = soul_token_array_type_back(&array);
		ASSERT_EQ(expected_token.type, actual_token.type);

		soul_token_array_destroy(&array);
	}

	TEST_F(TokenArrayTest, TypeBack_Empty)
	{
		array = soul_token_array_create();

		soul_token_t expected_token = {
			.type = soul_token_error,
			.start = NULL,
			.length = 0,
		};
		soul_token_array_append(&array, expected_token);
		ASSERT_NE(array.tokens, NULL);
		ASSERT_EQ(array.size, 1);

		soul_token_t actual_token = soul_token_array_type_back(&array);
		ASSERT_EQ(expected_token.type, actual_token.type);

		soul_token_array_destroy(&array);
	}
}
