#pragma once

// {} : braces
// [] : brackets
// () : parenthesis

enum TokenType {
  TOKEN_LEFT_PAREN,
  TOKEN_RIGHT_PAREN,
  TOKEN_LEFT_BRACKET,
  TOKEN_RIGHT_BRACKET,
  TOKEN_LEFT_BRACE,
  TOKEN_RIGHT_BRACE,
  TOKEN_COLON,
  TOKEN_DOT,
  TOKEN_DOTDOT,
  TOKEN_DOTDOTDOT,
  TOKEN_COMMA,
  TOKEN_STAR,
  TOKEN_SLASH,
  TOKEN_PERCENT,
  TOKEN_PLUS,
  TOKEN_MINUS,
  TOKEN_LTLT,
  TOKEN_GTGT,
  TOKEN_PIPE,
  TOKEN_PIPEPIPE,
  TOKEN_CARET,
  TOKEN_AMP,
  TOKEN_AMPAMP,
  TOKEN_BANG,
  TOKEN_TILDE,
  TOKEN_QUESTION,
  TOKEN_EQ,
  TOKEN_LT,
  TOKEN_GT,
  TOKEN_LTEQ,
  TOKEN_GTEQ,
  TOKEN_EQEQ,
  TOKEN_BANGEQ,

  TOKEN_BREAK,
  TOKEN_CLASS,
  TOKEN_CONSTRUCT,
  TOKEN_ELSE,
  TOKEN_FALSE,
  TOKEN_FOR,
  TOKEN_FOREIGN,
  TOKEN_IF,
  TOKEN_IMPORT,
  TOKEN_IN,
  TOKEN_IS,
  TOKEN_NULL,
  TOKEN_RETURN,
  TOKEN_STATIC,
  TOKEN_SUPER,
  TOKEN_THIS,
  TOKEN_TRUE,
  TOKEN_VAR,
  TOKEN_WHILE,

  TOKEN_FIELD,
  TOKEN_STATIC_FIELD,
  TOKEN_NAME,
  TOKEN_NUMBER,

  // A string literal without any interpolation, or the last section of a
  // string following the last interpolated expression.
  TOKEN_STRING,

  // A portion of a string literal preceding an interpolated expression. This
  // string:
  //
  //     "a %(b) c %(d) e"
  //
  // is tokenized to:
  //
  //     TOKEN_INTERPOLATION "a "
  //     TOKEN_NAME          b
  //     TOKEN_INTERPOLATION " c "
  //     TOKEN_NAME          d
  //     TOKEN_STRING        " e"
  TOKEN_INTERPOLATION,

  TOKEN_LINE,

  TOKEN_ERROR,
  TOKEN_EOF
};

struct Token {
  TokenType type;
  const char* start;
  int length;
  int line;
  Value value;
};

class Lexer {
 public:
  Lexer(const char* source, int source_length);

  void Init(const char* source, int source_length);


  // Member variables
 protected:
  const char* source;
  const char* tokenStart;
  const char* currentChar;
  int currentLine;
  Token current;
  Token previous;

  int braces[MAX_INTERPOLATION_NESTING];
  int numBraces;

  bool skip_new_lines;
  bool print_errors;
  bool has_errors;
};
