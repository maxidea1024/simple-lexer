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

  char Lexer::PeekChar();
  char Lexer::PeekNextChar();
  char Lexer::NextChar();
  bool Lexer::MatchChar(char c);
  void Lexer::MakeToken(TokenType type);
  void Lexer::TwoCharToken(char c, TokenType two, TokenType one);
  void Lexer::SkipLineComment();
  void Lexer::SkipBlockComment();
  int  Lexer::ReadHexDigit();
  void Lexer::MakeNumber(bool hex);
  void Lexer::ReadHexNumber();
  void Lexer::ReadNumber();
  void Lexer::ReadName(TokenType type);
  int  Lexer::ReadHexEscape(int digits, const char* tag);
  void Lexer::ReadUnicodeEscape(ByteBuffer* string, int length);
  void Lexer::ReadString();
  void Lexer::NextToken();
  void Lexer::LexError(const char* error, ...);

  // Member variables
 public:
  Token current;
  Token previous;

 protected:
  const char* source_;
  const char* token_start_;
  const char* current_char_;
  int current_line_;

  // The maximum depth that interpolation can nest. For example, this string has
  // three levels:
  //
  //      "outside %(one + "%(two + "%(three)")")"
  static const int MAX_INTERPOLATION_NESTING = 8;
  int braces_[MAX_INTERPOLATION_NESTING];
  int numBraces_;

  bool skip_new_lines_;
  bool print_errors_;
  bool has_errors_;
};
