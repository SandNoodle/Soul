// Single character tokens
TOKEN(TOKEN_SEMICOLON)
TOKEN(TOKEN_QUESTION_MARK)
TOKEN(TOKEN_DOT) TOKEN(TOKEN_COMMA)
TOKEN(TOKEN_PLUS) TOKEN(TOKEN_MINUS)
TOKEN(TOKEN_STAR) TOKEN(TOKEN_SLASH)
TOKEN(TOKEN_PAREN_LEFT) TOKEN(TOKEN_PAREN_RIGHT)
TOKEN(TOKEN_BRACE_LEFT) TOKEN(TOKEN_BRACE_RIGHT)
TOKEN(TOKEN_BRACKET_LEFT) TOKEN(TOKEN_BRACKET_RIGHT)

// One or two character tokens
TOKEN(TOKEN_COLON) TOKEN(TOKEN_DOUBLE_COLON)
TOKEN(TOKEN_EQUAL) TOKEN(TOKEN_DOUBLE_EQUAL)
TOKEN(TOKEN_BANG) TOKEN(TOKEN_BANG_EQUAL)
TOKEN(TOKEN_GREATER) TOKEN(TOKEN_GREATER_EQUAL)
TOKEN(TOKEN_LESS) TOKEN(TOKEN_LESS_EQUAL)

// Literals
TOKEN(TOKEN_NUMBER)
TOKEN(TOKEN_STRING)
TOKEN(TOKEN_IDENTIFIER)

// Keywords
TOKEN(TOKEN_NATIVE)                      // Native C function
TOKEN(TOKEN_IMPORT)                      // File import
TOKEN(TOKEN_DEFINE)                      // Defines
TOKEN(TOKEN_LET) TOKEN(TOKEN_MUT)        // Variables
TOKEN(TOKEN_IF) TOKEN(TOKEN_ELSE)        // Flow Control
TOKEN(TOKEN_FOR) TOKEN(TOKEN_WHILE)      // Loops
TOKEN(TOKEN_CONTINUE) TOKEN(TOKEN_BREAK) // Loop keywords
TOKEN(TOKEN_RETURN) TOKEN(TOKEN_FN)      // Function
TOKEN(TOKEN_STRUCT) TOKEN(TOKEN_ENUM)    // Data types
TOKEN(TOKEN_TRUE) TOKEN(TOKEN_FALSE)     // Truthness

// Special tokens
TOKEN(TOKEN_ERROR) TOKEN(TOKEN_EOF)

TOKEN(TOKEN_COUNT)

#undef TOKEN
