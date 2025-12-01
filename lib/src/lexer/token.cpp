#include "lexer/token.h"

#include <unordered_map>

namespace soul
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

	Token::operator std::string() const { return std::format("<{}:\"{}\">", internal_name(type), data); }

	std::string_view Token::name(Token::Type type) noexcept
	{
		static const std::unordered_map<Token::Type, std::string_view> k_token_name = {
			{ Token::Type::KeywordBreak,             "break"sv          },
			{ Token::Type::KeywordCast,              "cast"sv           },
			{ Token::Type::KeywordContinue,          "continue"sv       },
			{ Token::Type::KeywordElse,              "else"sv           },
			{ Token::Type::KeywordFalse,             "false"sv          },
			{ Token::Type::KeywordFn,                "fn"sv             },
			{ Token::Type::KeywordFor,               "for"sv            },
			{ Token::Type::KeywordIf,                "if"sv             },
			{ Token::Type::KeywordLet,               "let"sv            },
			{ Token::Type::KeywordMut,               "mut"sv            },
			{ Token::Type::KeywordNative,            "native"sv         },
			{ Token::Type::KeywordNull,              "null"sv           },
			{ Token::Type::KeywordReturn,            "return"sv         },
			{ Token::Type::KeywordStruct,            "struct"sv         },
			{ Token::Type::KeywordTrue,              "true"sv           },
			{ Token::Type::KeywordWhile,             "while"sv          },
			{ Token::Type::LiteralFloat,             "float literal"sv  },
			{ Token::Type::LiteralIdentifier,        "identifier"sv     },
			{ Token::Type::LiteralInteger,           "int literal"sv    },
			{ Token::Type::LiteralString,            "string literal"sv },
			{ Token::Type::SymbolAmpersand,          "&"sv              },
			{ Token::Type::SymbolAmpersandAmpersand, "&&"sv             },
			{ Token::Type::SymbolCaret,              "^"sv              },
			{ Token::Type::SymbolComma,              ","sv              },
			{ Token::Type::SymbolDot,                "."sv              },
			{ Token::Type::SymbolGreater,            ">"sv              },
			{ Token::Type::SymbolGreaterEqual,       ">="sv             },
			{ Token::Type::SymbolLess,               "<"sv              },
			{ Token::Type::SymbolLessEqual,          "<="sv             },
			{ Token::Type::SymbolMinus,              "-"sv              },
			{ Token::Type::SymbolMinusEqual,         "-="sv             },
			{ Token::Type::SymbolMinusMinus,         "--"sv             },
			{ Token::Type::SymbolPercent,            "%"sv              },
			{ Token::Type::SymbolPercentEqual,       "%="sv             },
			{ Token::Type::SymbolPipe,               "|"sv              },
			{ Token::Type::SymbolPipePipe,           "||"sv             },
			{ Token::Type::SymbolPlus,               "+"sv              },
			{ Token::Type::SymbolPlusEqual,          "+="sv             },
			{ Token::Type::SymbolPlusPlus,           "++"sv             },
			{ Token::Type::SymbolQuestionMark,       "?"sv              },
			{ Token::Type::SymbolSlash,              "/"sv              },
			{ Token::Type::SymbolSlashEqual,         "/="sv             },
			{ Token::Type::SymbolStar,               "*"sv              },
			{ Token::Type::SymbolStarEqual,          "*="sv             },
			{ Token::Type::SymbolBang,               "!"sv              },
			{ Token::Type::SymbolBangEqual,          "!="sv             },
			{ Token::Type::SymbolBraceLeft,          "{"sv              },
			{ Token::Type::SymbolBraceRight,         "}"sv              },
			{ Token::Type::SymbolBracketLeft,        "["sv              },
			{ Token::Type::SymbolBracketRight,       "]"sv              },
			{ Token::Type::SymbolColon,              ":"sv              },
			{ Token::Type::SymbolColonColon,         "::"sv             },
			{ Token::Type::SymbolEqual,              "="sv              },
			{ Token::Type::SymbolEqualEqual,         "=="sv             },
			{ Token::Type::SymbolParenLeft,          "("sv              },
			{ Token::Type::SymbolParenRight,         ")"sv              },
			{ Token::Type::SymbolSemicolon,          ";"sv              },
			{ Token::Type::SpecialError,             "__ERROR__"sv      },
			{ Token::Type::SpecialEndOfFile,         "__EOF__"sv        },
		};
		return k_token_name.at(type);
	}

	std::string_view Token::internal_name(Token::Type type) noexcept
	{
		static const std::unordered_map<Token::Type, std::string_view> k_token_name = {
			{ Token::Type::KeywordBreak,             "keyword_break"sv              },
			{ Token::Type::KeywordCast,              "keyword_cast"sv               },
			{ Token::Type::KeywordContinue,          "keyword_continue"sv           },
			{ Token::Type::KeywordElse,              "keyword_else"sv               },
			{ Token::Type::KeywordFalse,             "keyword_false"sv              },
			{ Token::Type::KeywordFn,                "keyword_fn"sv                 },
			{ Token::Type::KeywordFor,               "keyword_for"sv                },
			{ Token::Type::KeywordIf,                "keyword_if"sv                 },
			{ Token::Type::KeywordLet,               "keyword_let"sv                },
			{ Token::Type::KeywordMut,               "keyword_mut"sv                },
			{ Token::Type::KeywordNative,            "keyword_native"sv             },
			{ Token::Type::KeywordNull,              "keyword_null"sv               },
			{ Token::Type::KeywordReturn,            "keyword_return"sv             },
			{ Token::Type::KeywordStruct,            "keyword_struct"sv             },
			{ Token::Type::KeywordTrue,              "keyword_true"sv               },
			{ Token::Type::KeywordWhile,             "keyword_while"sv              },
			{ Token::Type::LiteralFloat,             "literal_float"sv              },
			{ Token::Type::LiteralIdentifier,        "literal_identifier"sv         },
			{ Token::Type::LiteralInteger,           "literal_integer"sv            },
			{ Token::Type::LiteralString,            "literal_string"sv             },
			{ Token::Type::SymbolAmpersand,          "symbol_ampersand"sv           },
			{ Token::Type::SymbolAmpersandAmpersand, "symbol_ampersand_ampersand"sv },
			{ Token::Type::SymbolCaret,              "symbol_caret"sv               },
			{ Token::Type::SymbolComma,              "symbol_comma"sv               },
			{ Token::Type::SymbolDot,                "symbol_dot"sv                 },
			{ Token::Type::SymbolGreater,            "symbol_greater"sv             },
			{ Token::Type::SymbolGreaterEqual,       "symbol_greater_equal"sv       },
			{ Token::Type::SymbolLess,               "symbol_less"sv                },
			{ Token::Type::SymbolLessEqual,          "symbol_less_equal"sv          },
			{ Token::Type::SymbolMinus,              "symbol_minus"sv               },
			{ Token::Type::SymbolMinusEqual,         "symbol_minus_equal"sv         },
			{ Token::Type::SymbolMinusMinus,         "symbol_minus_minus"sv         },
			{ Token::Type::SymbolPercent,            "symbol_percent"sv             },
			{ Token::Type::SymbolPercent,            "symbol_percent_equal"sv       },
			{ Token::Type::SymbolPipe,               "symbol_pipe"sv                },
			{ Token::Type::SymbolPipePipe,           "symbol_pipe_pipe"sv           },
			{ Token::Type::SymbolPlus,               "symbol_plus"sv                },
			{ Token::Type::SymbolPlusEqual,          "symbol_plus_equal"sv          },
			{ Token::Type::SymbolPlusPlus,           "symbol_plus_plus"sv           },
			{ Token::Type::SymbolQuestionMark,       "symbol_question_mark"sv       },
			{ Token::Type::SymbolSlash,              "symbol_slash"sv               },
			{ Token::Type::SymbolSlashEqual,         "symbol_slash_equal"sv         },
			{ Token::Type::SymbolStar,               "symbol_star"sv                },
			{ Token::Type::SymbolStarEqual,          "symbol_star_equal"sv          },
			{ Token::Type::SymbolBang,               "symbol_bang"sv                },
			{ Token::Type::SymbolBangEqual,          "symbol_bang_equal"sv          },
			{ Token::Type::SymbolBraceLeft,          "symbol_brace_left"sv          },
			{ Token::Type::SymbolBraceRight,         "symbol_brace_right"sv         },
			{ Token::Type::SymbolBracketLeft,        "symbol_bracket_left"sv        },
			{ Token::Type::SymbolBracketRight,       "symbol_bracket_right"sv       },
			{ Token::Type::SymbolColon,              "symbol_colon"sv               },
			{ Token::Type::SymbolColonColon,         "symbol_colon_colon"sv         },
			{ Token::Type::SymbolEqual,              "symbol_equal"sv               },
			{ Token::Type::SymbolEqualEqual,         "symbol_equal_equal"sv         },
			{ Token::Type::SymbolParenLeft,          "symbol_paren_left"sv          },
			{ Token::Type::SymbolParenRight,         "symbol_paren_right"sv         },
			{ Token::Type::SymbolSemicolon,          "symbol_semicolon"sv           },
			{ Token::Type::SpecialError,             "special_error"sv              },
			{ Token::Type::SpecialEndOfFile,         "special_eof"sv                },
		};
		return k_token_name.at(type);
	}
}  // namespace soul
