#include "Lexer/Token.h"

#include <unordered_map>

namespace Soul::Lexer
{
	using namespace std::string_view_literals;

	bool Token::operator==(const Token& other) const noexcept
	{
		return std::tie(type, data) == std::tie(other.type, other.data);
	}

	std::strong_ordering Token::operator<=>(const Token& other) const noexcept
	{
		return std::tie(type, data) <=> std::tie(other.type, other.data);
	}

	Token::operator std::string() const { return std::format("<{}:\"{}\">", NameInternal(type), data); }

	std::string_view Token::Name(Type type) noexcept
	{
		static const std::unordered_map<Type, std::string_view> k_token_name = {
			{ Type::KEYWORD_BREAK,              "break"sv          },
			{ Type::KEYWORD_CAST,               "cast"sv           },
			{ Type::KEYWORD_CONTINUE,           "continue"sv       },
			{ Type::KEYWORD_ELSE,               "else"sv           },
			{ Type::KEYWORD_FALSE,              "false"sv          },
			{ Type::KEYWORD_FN,                 "fn"sv             },
			{ Type::KEYWORD_FOR,                "for"sv            },
			{ Type::KEYWORD_IF,                 "if"sv             },
			{ Type::KEYWORD_LET,                "let"sv            },
			{ Type::KEYWORD_MUT,                "mut"sv            },
			{ Type::KEYWORD_NATIVE,             "native"sv         },
			{ Type::KEYWORD_NULL,               "null"sv           },
			{ Type::KEYWORD_RETURN,             "return"sv         },
			{ Type::KEYWORD_STRUCT,             "struct"sv         },
			{ Type::KEYWORD_TRUE,               "true"sv           },
			{ Type::KEYWORD_WHILE,              "while"sv          },
			{ Type::LITERAL_FLOAT,              "float literal"sv  },
			{ Type::LITERAL_IDENTIFIER,         "identifier"sv     },
			{ Type::LITERAL_INTEGER,            "int literal"sv    },
			{ Type::LITERAL_STRING,             "string literal"sv },
			{ Type::SYMBOL_AMPERSAND,           "&"sv              },
			{ Type::SYMBOL_AMPERSAND_AMPERSAND, "&&"sv             },
			{ Type::SYMBOL_CARET,               "^"sv              },
			{ Type::SYMBOL_COMMA,               ","sv              },
			{ Type::SYMBOL_DOT,                 "."sv              },
			{ Type::SYMBOL_GREATER,             ">"sv              },
			{ Type::SYMBOL_GREATER_EQUAL,       ">="sv             },
			{ Type::SYMBOL_LESS,                "<"sv              },
			{ Type::SYMBOL_LESS_EQUAL,          "<="sv             },
			{ Type::SYMBOL_MINUS,               "-"sv              },
			{ Type::SYMBOL_MINUS_EQUAL,         "-="sv             },
			{ Type::SYMBOL_MINUS_MINUS,         "--"sv             },
			{ Type::SYMBOL_PERCENT,             "%"sv              },
			{ Type::SYMBOL_PERCENT_EQUAL,       "%="sv             },
			{ Type::SYMBOL_PIPE,                "|"sv              },
			{ Type::SYMBOL_PIPE_PIPE,           "||"sv             },
			{ Type::SYMBOL_PLUS,                "+"sv              },
			{ Type::SYMBOL_PLUS_EQUAL,          "+="sv             },
			{ Type::SYMBOL_PLUS_PLUS,           "++"sv             },
			{ Type::SYMBOL_QUESTION_MARK,       "?"sv              },
			{ Type::SYMBOL_SLASH,               "/"sv              },
			{ Type::SYMBOL_SLASH_EQUAL,         "/="sv             },
			{ Type::SYMBOL_STAR,                "*"sv              },
			{ Type::SYMBOL_STAR_EQUAL,          "*="sv             },
			{ Type::SYMBOL_BANG,                "!"sv              },
			{ Type::SYMBOL_BANG_EQUAL,          "!="sv             },
			{ Type::SYMBOL_BRACE_LEFT,          "{"sv              },
			{ Type::SYMBOL_BRACE_RIGHT,         "}"sv              },
			{ Type::SYMBOL_BRACKET_LEFT,        "["sv              },
			{ Type::SYMBOL_BRACKET_RIGHT,       "]"sv              },
			{ Type::SYMBOL_COLON,               ":"sv              },
			{ Type::SYMBOL_COLON_COLON,         "::"sv             },
			{ Type::SYMBOL_EQUAL,               "="sv              },
			{ Type::SYMBOL_EQUAL_EQUAL,         "=="sv             },
			{ Type::SYMBOL_PAREN_LEFT,          "("sv              },
			{ Type::SYMBOL_PAREN_RIGHT,         ")"sv              },
			{ Type::SYMBOL_SEMICOLON,           ";"sv              },
			{ Type::SPECIAL_ERROR,              "__ERROR__"sv      },
			{ Type::SPECIAL_END_OF_FILE,        "__EOF__"sv        },
		};
		return k_token_name.at(type);
	}

	std::string_view Token::NameInternal(Type type) noexcept
	{
		static const std::unordered_map<Type, std::string_view> k_token_name = {
			{ Type::KEYWORD_BREAK,              "keyword_break"sv              },
			{ Type::KEYWORD_CAST,               "keyword_cast"sv               },
			{ Type::KEYWORD_CONTINUE,           "keyword_continue"sv           },
			{ Type::KEYWORD_ELSE,               "keyword_else"sv               },
			{ Type::KEYWORD_FALSE,              "keyword_false"sv              },
			{ Type::KEYWORD_FN,                 "keyword_fn"sv                 },
			{ Type::KEYWORD_FOR,                "keyword_for"sv                },
			{ Type::KEYWORD_IF,                 "keyword_if"sv                 },
			{ Type::KEYWORD_LET,                "keyword_let"sv                },
			{ Type::KEYWORD_MUT,                "keyword_mut"sv                },
			{ Type::KEYWORD_NATIVE,             "keyword_native"sv             },
			{ Type::KEYWORD_NULL,               "keyword_null"sv               },
			{ Type::KEYWORD_RETURN,             "keyword_return"sv             },
			{ Type::KEYWORD_STRUCT,             "keyword_struct"sv             },
			{ Type::KEYWORD_TRUE,               "keyword_true"sv               },
			{ Type::KEYWORD_WHILE,              "keyword_while"sv              },
			{ Type::LITERAL_FLOAT,              "literal_float"sv              },
			{ Type::LITERAL_IDENTIFIER,         "literal_identifier"sv         },
			{ Type::LITERAL_INTEGER,            "literal_integer"sv            },
			{ Type::LITERAL_STRING,             "literal_string"sv             },
			{ Type::SYMBOL_AMPERSAND,           "symbol_ampersand"sv           },
			{ Type::SYMBOL_AMPERSAND_AMPERSAND, "symbol_ampersand_ampersand"sv },
			{ Type::SYMBOL_CARET,               "symbol_caret"sv               },
			{ Type::SYMBOL_COMMA,               "symbol_comma"sv               },
			{ Type::SYMBOL_DOT,                 "symbol_dot"sv                 },
			{ Type::SYMBOL_GREATER,             "symbol_greater"sv             },
			{ Type::SYMBOL_GREATER_EQUAL,       "symbol_greater_equal"sv       },
			{ Type::SYMBOL_LESS,                "symbol_less"sv                },
			{ Type::SYMBOL_LESS_EQUAL,          "symbol_less_equal"sv          },
			{ Type::SYMBOL_MINUS,               "symbol_minus"sv               },
			{ Type::SYMBOL_MINUS_EQUAL,         "symbol_minus_equal"sv         },
			{ Type::SYMBOL_MINUS_MINUS,         "symbol_minus_minus"sv         },
			{ Type::SYMBOL_PERCENT,             "symbol_percent"sv             },
			{ Type::SYMBOL_PERCENT,             "symbol_percent_equal"sv       },
			{ Type::SYMBOL_PIPE,                "symbol_pipe"sv                },
			{ Type::SYMBOL_PIPE_PIPE,           "symbol_pipe_pipe"sv           },
			{ Type::SYMBOL_PLUS,                "symbol_plus"sv                },
			{ Type::SYMBOL_PLUS_EQUAL,          "symbol_plus_equal"sv          },
			{ Type::SYMBOL_PLUS_PLUS,           "symbol_plus_plus"sv           },
			{ Type::SYMBOL_QUESTION_MARK,       "symbol_question_mark"sv       },
			{ Type::SYMBOL_SLASH,               "symbol_slash"sv               },
			{ Type::SYMBOL_SLASH_EQUAL,         "symbol_slash_equal"sv         },
			{ Type::SYMBOL_STAR,                "symbol_star"sv                },
			{ Type::SYMBOL_STAR_EQUAL,          "symbol_star_equal"sv          },
			{ Type::SYMBOL_BANG,                "symbol_bang"sv                },
			{ Type::SYMBOL_BANG_EQUAL,          "symbol_bang_equal"sv          },
			{ Type::SYMBOL_BRACE_LEFT,          "symbol_brace_left"sv          },
			{ Type::SYMBOL_BRACE_RIGHT,         "symbol_brace_right"sv         },
			{ Type::SYMBOL_BRACKET_LEFT,        "symbol_bracket_left"sv        },
			{ Type::SYMBOL_BRACKET_RIGHT,       "symbol_bracket_right"sv       },
			{ Type::SYMBOL_COLON,               "symbol_colon"sv               },
			{ Type::SYMBOL_COLON_COLON,         "symbol_colon_colon"sv         },
			{ Type::SYMBOL_EQUAL,               "symbol_equal"sv               },
			{ Type::SYMBOL_EQUAL_EQUAL,         "symbol_equal_equal"sv         },
			{ Type::SYMBOL_PAREN_LEFT,          "symbol_paren_left"sv          },
			{ Type::SYMBOL_PAREN_RIGHT,         "symbol_paren_right"sv         },
			{ Type::SYMBOL_SEMICOLON,           "symbol_semicolon"sv           },
			{ Type::SPECIAL_ERROR,              "special_error"sv              },
			{ Type::SPECIAL_END_OF_FILE,        "special_eof"sv                },
		};
		return k_token_name.at(type);
	}
}  // namespace Soul::Lexer
