#pragma once

#include <vector>
#include <string>

#include "value.h"

// {} : braces
// [] : brackets
// () : parenthesis


/*
static void stringInterpolation(Compiler* compiler, bool canAssign) {
  // Instantiate a new list.
  loadCoreVariable(compiler, "List");
  callMethod(compiler, 0, "new()", 5);

  do {
    // The opening string part.
    literal(compiler, false);
    callMethod(compiler, 1, "addCore_(_)", 11);

    // The interpolated expression.
    ignoreNewlines(compiler);
    expression(compiler);
    callMethod(compiler, 1, "addCore_(_)", 11);

    ignoreNewlines(compiler);
  } while (match(compiler, TOKEN_INTERPOLATION));

  // The trailing string part.
  consume(compiler, TOKEN_STRING, "Expect end of string interpolation.");
  literal(compiler, false);
  callMethod(compiler, 1, "addCore_(_)", 11);

  // The list of interpolated parts.
  callMethod(compiler, 0, "join()", 6);
}
*/


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
  TOKEN_DOTDOTLT,
  TOKEN_COMMA,

  TOKEN_STAR,
  TOKEN_STARSTAR,

  TOKEN_SLASH,
  TOKEN_PERCENT,
  TOKEN_PLUS,
  TOKEN_PLUSPLUS,
  TOKEN_MINUS,
  TOKEN_MINUSMINUS,
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

  //TODO
  //TOKEN_EQEQEQ,
  //TOKEN_BANGEQEQEQ,

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

  std::string TypeName() const;
  std::string ToString() const;
};

class Lexer {
 public:
  Lexer();
  Lexer(const char* source);
  Lexer(const char* source, size_t source_length);

  void Init(const char* source);
  void Init(const char* source, size_t source_length);

  bool IsEOF() const;

  char PeekChar() const;
  char PeekNextChar() const;
  char NextChar();
  bool MatchChar(char c);
  void MakeToken(TokenType type);
  void TwoCharToken(char c, TokenType two, TokenType one);
  void SkipLineComment();
  void SkipBlockComment();
  int ReadHexDigit();
  void MakeNumber(bool hex);
  void ReadHexNumber();
  void ReadNumber();
  void ReadName(TokenType type);
  int ReadHexEscape(int digits, const char* tag);
  void ReadUnicodeEscape(std::vector<char>* string, int length);
  void ReadString();
  void NextToken();
  void LexError(const char* error, ...);

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
  int num_braces_;

  bool skip_new_lines_;
  bool print_errors_;
  bool has_errors_;
};
