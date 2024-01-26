#include "lexer/lexer.h"

#include <gtest/gtest.h>

// TODO: This Module Test should load all the ".soul" files from a test/data/
//       directory and verify if given case should fail or not given its suffix.
namespace soul::mt
{
	class LexerTest : public ::testing::TestWithParam<Case>
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
	};
}
