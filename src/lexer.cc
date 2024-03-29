#ifdef _MSC_VER
#pragma warning(disable : 4819)
#endif

#include <assert.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>

#include "lexer.h"
#include "strings.h"
#include "util.h"

struct Keyword {
  const char* identifier;
  size_t length;
  TokenType token_type;
};

// The table of reserved words and their associated token types.
static Keyword keywords[] = {
    {"break", 5, TOKEN_BREAK},
    {"class", 5, TOKEN_CLASS},
    {"construct", 9, TOKEN_CONSTRUCT},
    {"else", 4, TOKEN_ELSE},
    {"false", 5, TOKEN_FALSE},
    {"for", 3, TOKEN_FOR},
    {"foreign", 7, TOKEN_FOREIGN},
    {"if", 2, TOKEN_IF},
    {"import", 6, TOKEN_IMPORT},
    {"in", 2, TOKEN_IN},
    {"is", 2, TOKEN_IS},
    {"null", 4, TOKEN_NULL},
    {"return", 6, TOKEN_RETURN},
    {"static", 6, TOKEN_STATIC},
    {"super", 5, TOKEN_SUPER},
    {"this", 4, TOKEN_THIS},
    {"true", 4, TOKEN_TRUE},
    {"var", 3, TOKEN_VAR},
    {"while", 5, TOKEN_WHILE},
    {NULL, 0, TOKEN_EOF}  // Sentinel to mark the end of the array.
};

Lexer::Lexer() { Init("", 0); }

Lexer::Lexer(const char* source) { Init(source, strlen(source)); }

Lexer::Lexer(const char* source, size_t source_length) {
  Init(source, source_length);
}

void Lexer::Init(const char* source) { Init(source, strlen(source)); }

void Lexer::Init(const char* source, size_t source_length) {
  source_ = source;
  token_start_ = source;
  current_char_ = source;
  current_line_ = 1;
  current_column_ = 1;
  num_interp_braces_ = 0;

  tab_size_ = 2;

  current.type = TOKEN_ERROR;
  current.start = source;
  current.length = 0;
  current.line = 0;
  current.column = 0;
  current.value = Value::NONE;

  // Ignore leading newlines.
  skip_new_lines_ = true;
  print_errors_ = true;
  has_errors_ = false;

  // Read the first token.
  NextToken();
}

bool Lexer::IsEOF() const { return current.type == TOKEN_EOF; }

bool Lexer::HasErrors() const { return has_errors_; }

bool Lexer::HasNextToken() const { return !IsEOF() && !HasErrors(); }

// Returns true if [c] is a valid (non-initial) identifier character.
inline bool IsName(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

// Returns true if [c] is a digit.
inline bool IsDigit(char c) { return c >= '0' && c <= '9'; }

// Returns the current character the lexer is sitting on.
char Lexer::PeekChar() const { return *current_char_; }

// Returns the character after the current character.
char Lexer::PeekNextChar() const {
  // If we're at the end of the source, don't read past it.
  if (PeekChar() == '\0') {
    return '\0';
  }

  return *(current_char_ + 1);
}

// Advances the lexer forward one character.
char Lexer::NextChar() {
  char c = PeekChar();
  // assert(*current_char_);
  current_char_++;
  if (c == '\n') {
    current_line_++;
    current_column_ = 1;
  } else if (c == '\t') {
    current_column_ += tab_size_;
  } else {
    current_column_++;
  }

  return c;
}

char Lexer::UngetChar() {
  assert(current_char_ > source_);

  current_char_--;
  if (*current_char_ == '\n') {
    current_line_--;
    // FIXME: 이전줄의 끝의 컬럼 번호로 해주어야함!
    // FIXME: 이전줄의 끝의 컬럼 번호로 해주어야함!
    // FIXME: 이전줄의 끝의 컬럼 번호로 해주어야함!
    // FIXME: 이전줄의 끝의 컬럼 번호로 해주어야함!
    // FIXME: 이전줄의 끝의 컬럼 번호로 해주어야함!
  } else if (*current_char_ == '\t') {
    current_column_ -= tab_size_;
  } else {
    current_column_--;
  }

  return *current_char_;
}

// If the current character is [c], consumes it and returns `true`.
bool Lexer::MatchChar(char c) {
  if (PeekChar() != c) {
    return false;
  }

  NextChar();
  return true;
}

// Sets the lexer's current token to the given [type] and current character
// range.
void Lexer::MakeToken(TokenType type) {
  current.type = type;
  current.start = token_start_;
  current.length = (int)(current_char_ - token_start_);
  current.line = current_line_;
  current.column = current_column_;

  // Make line tokens appear on the line containing the "\n".
  if (type == TOKEN_LINE) {
    current.line--;
  }
}

// If the current character is [c], then consumes it and makes a token of type
// [two]. Otherwise makes a token of type [one].
void Lexer::TwoCharToken(char c, TokenType two, TokenType one) {
  MakeToken(MatchChar(c) ? two : one);
}

// Skips the rest of the current line.
void Lexer::SkipLineComment() {
  while (PeekChar() != '\n' && PeekChar() != '\0') {
    NextChar();
  }
}

// Skips the rest of a block comment.
void Lexer::SkipBlockComment() {
  int nesting = 1;
  while (nesting > 0) {
    if (PeekChar() == '\0') {
      LexError("Unterminated block comment.");
      return;
    }

    if (PeekChar() == '/' && PeekNextChar() == '*') {
      NextChar();
      NextChar();
      nesting++;
      continue;
    }

    if (PeekChar() == '*' && PeekNextChar() == '/') {
      NextChar();
      NextChar();
      nesting--;
      continue;
    }

    // Regular comment characters.
    NextChar();
  }
}

// Reads the next character, which should be a hex digit (0-9, a-f, or A-F) and
// returns its numeric value. If the character isn't a hex digit, returns -1.
int Lexer::ReadHexDigit() {
  char c = NextChar();

  if (c >= '0' && c <= '9') {
    return c - '0';
  }

  if (c >= 'a' && c <= 'f') {
    return c - 'a' + 10;
  }

  if (c >= 'A' && c <= 'F') {
    return c - 'A' + 10;
  }

  // Don't consume it if it isn't expected. Keeps us from reading past the end
  // of an unterminated string.
  // current_char_--;
  UngetChar();
  return -1;
}

void Lexer::MakeNumber(bool hex) {
  errno = 0;

  if (hex) {
    current.value = Value((double)strtoll(token_start_, NULL, 16));
  } else {
    current.value = Value(strtod(token_start_, NULL));
  }

  if (errno == ERANGE) {
    LexError("Number literal was too large (%d).", sizeof(long int));
    current.value = Value((int64_t)0);
  }

  // We don't check that the entire token is consumed after calling strtoll()
  // or strtod() because we've already scanned it ourselves and know it's valid.

  MakeToken(TOKEN_NUMBER);
}

void Lexer::ReadHexNumber() {
  // Skip past the `x` used to denote a hexadecimal literal.
  NextChar();

  // Iterate over all the valid hexadecimal digits found.
  while (ReadHexDigit() != -1)
    ;

  MakeNumber(true);
}

void Lexer::ReadNumber() {
  if (IsDigit(PeekChar())) {
    NextChar();
  }

  // See if it has a floating point. Make sure there is a digit after the "."
  // so we don't get confused by method calls on number literals.
  if (PeekChar() == '.' && IsDigit(PeekNextChar())) {
    NextChar();
    while (IsDigit(PeekChar())) {
      NextChar();
    }
  }

  // See if the number is in scientific notation.
  if (MatchChar('e') || MatchChar('E')) {
    // Allow a negative exponent.
    MatchChar('-');

    if (!IsDigit(PeekChar())) {
      LexError("Unterminated scientific notation.");
    }

    while (IsDigit(PeekChar())) {
      NextChar();
    }
  }

  MakeNumber(false);
}

// Finishes lexing an identifier. Handles reserved words.
void Lexer::ReadName(TokenType type) {
  while (IsName(PeekChar()) || IsDigit(PeekChar())) {
    NextChar();
  }

  // Update the type if it's a keyword.
  size_t length = current_char_ - token_start_;
  for (int i = 0; keywords[i].identifier != nullptr; ++i) {
    if (length == keywords[i].length &&
        memcmp(token_start_, keywords[i].identifier, length) == 0) {
      type = keywords[i].token_type;
      break;
    }
  }

  MakeToken(type);
}

// Reads [digits] hex digits in a string literal and returns their number value.
int Lexer::ReadHexEscape(int digits, const char* tag) {
  int value = 0;

  for (int i = 0; i < digits; ++i) {
    if (PeekChar() == '"' || PeekChar() == '\0') {
      LexError("Incomplete %s escape sequence.", tag);

      // Don't consume it if it isn't expected. Keeps us from reading past the
      // end of an unterminated string.
      // current_char_--;
      UngetChar();
      break;
    }

    int digit = ReadHexDigit();
    if (digit == -1) {
      LexError("Invalid %s escape sequence.", tag);
      break;
    }

    value = (value * 16) | digit;
  }

  return value;
}

// Reads a hex digit Unicode escape sequence in a string literal.
void Lexer::ReadUnicodeEscape(std::vector<char>* string, int length) {
  int value = ReadHexEscape(length, "Unicode");

  // Grow the buffer enough for the encoded result.
  int num_bytes = Utf8EncodeNumBytes(value);
  if (num_bytes > 0) {
    string->resize(string->size() + num_bytes);
    Utf8Encode(value, (uint8_t*)&(*string)[0] + string->size() - num_bytes);
  }
}

inline void AppendChar(std::vector<char>* string, char ch) {
  string->push_back(ch);
}

void Lexer::PrepareReadString(char quote_char, bool force_verbatim) {
  read_string_quote_char_ = quote_char;

  read_string_quote_count_ = 1;

  // quote 다음에 라인피드가 있을 경우에는 어떤식으로 처리해야할까?

  if (MatchChar(quote_char)) {
    if (MatchChar(quote_char)) {
      read_string_quote_count_ = 3;
    } else {
      // current_char_--;
      UngetChar();
    }
  }

  read_string_verbatim_ = (force_verbatim || read_string_quote_count_ == 3);
}

// Finishes lexing a string literal.
void Lexer::ReadString() {
  std::vector<char> string;
  TokenType type = TOKEN_STRING;

  for (;;) {
    char c = NextChar();
    if (c == read_string_quote_char_) {
      if (read_string_quote_count_ == 1) {
        break;
      } else {
        // three quotes
        // warning:
        // quote 문자 뒤에 라인피드가 오면 문제가 발생할 수 있음!
        // 이부분에 대해서는 보완이 필요함.
        if (MatchChar(read_string_quote_char_)) {
          if (MatchChar(read_string_quote_char_)) {
            break;
          } else {
            // current_char_--;
            UngetChar();
          }
        }
      }
    }

    if (c == '\0') {
      LexError("Unterminated string.");

      // Don't consume it if it isn't expected. Keeps us from reading past the
      // end of an unterminated string.
      // current_char_--;
      UngetChar();
      break;
    }

    if (c == '\n') {
      if (!read_string_verbatim_) {
        LexError("EOL while scanning string literal");
        return;
      }
    }

    if (c == '$') {
      if (num_interp_braces_ < MAX_INTERPOLATION_NESTING) {
        // TODO: Allow format string.
        // 어떻게 처리하지??

        // C#은 다음과 같음
        //   {<interpolationExpression>[,<alignment>][:<formatString>]}

        if (NextChar() != '{') {
          LexError("Expect '{' after '$'.");
        }

        interp_braces_[num_interp_braces_++] = 1;
        type = TOKEN_INTERPOLATION;
        break;
      }

      LexError("Interpolation may only nest %d levels deep.",
               MAX_INTERPOLATION_NESTING);
    }

    if (c == '\\') {
      char c2 = NextChar();

      if (read_string_verbatim_) {
        AppendChar(&string, '\\');
        AppendChar(&string, c2);
        continue;
      }

      switch (c2) {
        case '"':
          AppendChar(&string, '"');
          break;
        case '\\':
          AppendChar(&string, '\\');
          break;
        case '%':
          AppendChar(&string, '%');
          break;
        case '0':
          AppendChar(&string, '\0');
          break;
        case 'a':
          AppendChar(&string, '\a');
          break;
        case 'b':
          AppendChar(&string, '\b');
          break;
        case 'f':
          AppendChar(&string, '\f');
          break;
        case 'n':
          AppendChar(&string, '\n');
          break;
        case 'r':
          AppendChar(&string, '\r');
          break;
        case 't':
          AppendChar(&string, '\t');
          break;
        case 'u':
          ReadUnicodeEscape(&string, 4);
          break;
        case 'U':
          ReadUnicodeEscape(&string, 8);
          break;
        case 'v':
          AppendChar(&string, '\v');
          break;
        case 'x':
          AppendChar(&string, (uint8_t)ReadHexEscape(2, "byte"));
          break;

        default:
          LexError("Invalid escape character '%c'.", *(current_char_ - 1));
          break;
      }
    } else {
      AppendChar(&string, c);
    }
  }

  if (string.size() > 0) {
    current.value = Value(&string[0], string.size());
  } else {
    current.value = Value("", 0);
  }
  string.clear();

  MakeToken(type);
}

void Lexer::NextToken() {
  previous = current;

  if (current.type == TOKEN_EOF) {
    return;
  }

  while (PeekChar() != '\0') {
    token_start_ = current_char_;

    char c = NextChar();
    switch (c) {
      case '(':
        MakeToken(TOKEN_LEFT_PAREN);
        return;

      case ')':
        MakeToken(TOKEN_RIGHT_PAREN);
        return;

      case '[':
        MakeToken(TOKEN_LEFT_BRACKET);
        return;
      case ']':
        MakeToken(TOKEN_RIGHT_BRACKET);
        return;

      case '{':
        if (num_interp_braces_ > 0) {
          interp_braces_[num_interp_braces_ - 1]++;
        }
        MakeToken(TOKEN_LEFT_BRACE);
        return;

      case '}':
        if (num_interp_braces_ > 0 &&
            --interp_braces_[num_interp_braces_ - 1] == 0) {
          // This is the final "}", so the interpolation expression has ended.
          // This "}" now begins the next section of the template string.
          num_interp_braces_--;
          ReadString();
          return;
        }
        MakeToken(TOKEN_RIGHT_BRACE);
        return;

      case ':':
        MakeToken(TOKEN_COLON);
        return;
      case ',':
        MakeToken(TOKEN_COMMA);
        return;
      case '*':
        TwoCharToken('*', TOKEN_STARSTAR, TOKEN_STAR);
        return;
      case '%':
        MakeToken(TOKEN_PERCENT);
        return;
      case '^':
        MakeToken(TOKEN_CARET);
        return;
      case '+':
        TwoCharToken('+', TOKEN_PLUSPLUS, TOKEN_PLUS);
        return;
      case '-':
        TwoCharToken('-', TOKEN_MINUSMINUS, TOKEN_MINUS);
        return;
      case '~':
        MakeToken(TOKEN_TILDE);
        return;
      case '?':
        MakeToken(TOKEN_QUESTION);
        return;

      case '|':
        TwoCharToken('|', TOKEN_PIPEPIPE, TOKEN_PIPE);
        return;
      case '&':
        TwoCharToken('&', TOKEN_AMPAMP, TOKEN_AMP);
        return;
      case '=':
        TwoCharToken('=', TOKEN_EQEQ, TOKEN_EQ);
        return;
      case '!':
        TwoCharToken('=', TOKEN_BANGEQ, TOKEN_BANG);
        return;

      case '.':
        if (MatchChar('.')) {
          if (MatchChar('<')) {
            MakeToken(TOKEN_DOTDOTLT);
          } else {
            TwoCharToken('.', TOKEN_DOTDOTDOT, TOKEN_DOTDOT);
          }
          return;
        }

        MakeToken(TOKEN_DOT);
        return;

      case '/':
        if (MatchChar('/')) {
          SkipLineComment();
          break;
        }

        if (MatchChar('*')) {
          SkipBlockComment();
          break;
        }

        MakeToken(TOKEN_SLASH);
        return;

      case '<':
        if (MatchChar('<')) {
          MakeToken(TOKEN_LTLT);
        } else {
          TwoCharToken('=', TOKEN_LTEQ, TOKEN_LT);
        }
        return;

      case '>':
        if (MatchChar('>')) {
          MakeToken(TOKEN_GTGT);
        } else {
          TwoCharToken('=', TOKEN_GTEQ, TOKEN_GT);
        }
        return;

      case '\n':
        MakeToken(TOKEN_LINE);
        return;

      case ' ':
      case '\r':
      case '\t':
        // Skip forward until we run out of whitespace.
        while (PeekChar() == ' ' || PeekChar() == '\r' || PeekChar() == '\t') {
          NextChar();
        }
        break;

      case '"':
        PrepareReadString('"', false);
        ReadString();
        return;
      case '\'':
        PrepareReadString('\'', false);
        ReadString();
        return;
      case '`':
        PrepareReadString('`', true);
        ReadString();
        return;

      // TODO: 이건 오바 아닌가??
      case '_':
        ReadName(PeekChar() == '_' ? TOKEN_STATIC_FIELD : TOKEN_FIELD);
        return;

      case '0':
        if (PeekChar() == 'x') {
          ReadHexNumber();
        } else {
          ReadNumber();
        }
        return;

      default:
        if (current_line_ == 1 && c == '#' && PeekChar() == '!') {
          // Ignore shebang on the first line.
          SkipLineComment();
          break;
        }

        if (IsName(c)) {
          ReadName(TOKEN_NAME);
        } else if (IsDigit(c)) {
          ReadNumber();
        } else {
          if (c >= 32 && c <= 126) {
            LexError("Invalid character '%c'.", c);
          } else {
            // Don't show non-ASCII values since we didn't UTF-8 decode the
            // bytes. Since there are no non-ASCII byte values that are
            // meaningful code units in Wren, the lexer works on raw bytes,
            // even though the source code and console output are UTF-8.
            LexError("Invalid byte 0x%x.", (uint8_t)c);
          }

          current.type = TOKEN_ERROR;
          current.length = 0;
        }

        return;
    }
  }

  // If we get here, we're out of source, so just make EOF tokens.
  token_start_ = current_char_;
  MakeToken(TOKEN_EOF);
}

void Lexer::LexError(const char* error, ...) {
  char buf[2048];

  va_list arg_list;
  va_start(arg_list, error);
  vsnprintf_s(buf, 2048 - 1, error, arg_list);
  va_end(arg_list);

  // TODO 그냥 마지막 에러 메시지를 저장하는 정도만 하면 되지 않으려나??
  //마지막 에러 내용을 저장하고 있어야함!

  has_errors_ = true;

  if (print_errors_) {
    fprintf(stderr, "LEX-ERROR(#%d:%d): %s\n", current.line, current.column, buf);
    fflush(stderr);
  }
}



/*



void Lexer::LexError(const char* format, ...) {
  va_list va;
  va_start(va, format);
  PrintError(current_line_, "Error", format, va);
  va_end(va);
}

void Compiler::Error(const char* format, ...) {
  Token& token = previous;

  // If the parse error was caused by an error token, the lexer has already report it.
  if (token.type == TOKEN_ERROR) {
    return;
  }
  
  va_list va;
  va_start(va, format);
  if (token.type == TOKEN_LINE) {
    PrintError(token.line, "Error at newline", format, va);
  } else if (token.type == TOKEN_EOF) {
    PrintError(token.line, "Error at end of file", format, va);
  } else {
    char label[10 + MAX_VARIABLE_NAME + 4 + 1];
    if (token.length <= MAX_VARIABLE_NAME) {
      sprintf(label, "Error at '%.*s'", token.length, token.start);
    } else {
      sprintf(label, "Error at '%.*s...'", MAX_VARIABLE_NAME, token.start);
    }
    PrintError(token.line, label, format, va);
  }
  va_end(va);
}
*/


std::string Token::TypeName() const {
  switch (type) {
    case TOKEN_LEFT_PAREN:
      return "(";
    case TOKEN_RIGHT_PAREN:
      return ")";
    case TOKEN_LEFT_BRACKET:
      return "[";
    case TOKEN_RIGHT_BRACKET:
      return "]";
    case TOKEN_LEFT_BRACE:
      return "{";
    case TOKEN_RIGHT_BRACE:
      return "}";
    case TOKEN_COLON:
      return ":";
    case TOKEN_DOT:
      return ".";
    case TOKEN_DOTDOT:
      return "..";
    case TOKEN_DOTDOTDOT:
      return "...";
    case TOKEN_DOTDOTLT:
      return "..<";
    case TOKEN_COMMA:
      return ",";
    case TOKEN_STAR:
      return "*";
    case TOKEN_STARSTAR:
      return "**";
    case TOKEN_SLASH:
      return "/";
    case TOKEN_PERCENT:
      return "%";
    case TOKEN_PLUS:
      return "+";
    case TOKEN_PLUSPLUS:
      return "++";
    case TOKEN_MINUS:
      return "-";
    case TOKEN_MINUSMINUS:
      return "--";
    case TOKEN_LTLT:
      return "<<";
    case TOKEN_GTGT:
      return ">>";
    case TOKEN_PIPE:
      return "|";
    case TOKEN_PIPEPIPE:
      return "||";
    case TOKEN_CARET:
      return "^";
    case TOKEN_AMP:
      return "&";
    case TOKEN_AMPAMP:
      return "&&";
    case TOKEN_BANG:
      return "!";
    case TOKEN_TILDE:
      return "~";
    case TOKEN_QUESTION:
      return "?";
    case TOKEN_EQ:
      return "=";
    case TOKEN_LT:
      return "<";
    case TOKEN_GT:
      return ">";
    case TOKEN_LTEQ:
      return "<=";
    case TOKEN_GTEQ:
      return ">=";
    case TOKEN_EQEQ:
      return "==";
    case TOKEN_BANGEQ:
      return "!=";

    case TOKEN_BREAK:
      return "break";
    case TOKEN_CLASS:
      return "class";
    case TOKEN_CONSTRUCT:
      return "construct";
    case TOKEN_ELSE:
      return "else";
    case TOKEN_FALSE:
      return "false";
    case TOKEN_FOR:
      return "for";
    case TOKEN_FOREIGN:
      return "foreign";
    case TOKEN_IF:
      return "if";
    case TOKEN_IMPORT:
      return "import";
    case TOKEN_IN:
      return "in";
    case TOKEN_IS:
      return "is";
    case TOKEN_NULL:
      return "null";
    case TOKEN_RETURN:
      return "return";
    case TOKEN_STATIC:
      return "static";
    case TOKEN_SUPER:
      return "super";
    case TOKEN_THIS:
      return "this";
    case TOKEN_TRUE:
      return "true";
    case TOKEN_VAR:
      return "var";
    case TOKEN_WHILE:
      return "while";

    case TOKEN_FIELD:
      return "field";
    case TOKEN_STATIC_FIELD:
      return "static_field";
    case TOKEN_NAME:
      return "name";
    case TOKEN_NUMBER:
      return "number";

    case TOKEN_STRING:
      return "string";

    case TOKEN_INTERPOLATION:
      return "interpolation";

    case TOKEN_LINE:
      return "line";

    case TOKEN_ERROR:
      return "error";
    case TOKEN_EOF:
      return "eof";
  }

  return std::string("__unknown__");
}

std::string Token::ToString() const {
  std::string ret = TypeName();

  switch (type) {
    case TOKEN_NUMBER:
      ret += " : ";
      ret += value.ToString();
      break;

    case TOKEN_STRING:
    case TOKEN_INTERPOLATION:
      ret += " : '";
      ret += value.ToString();
      ret += "'";
      break;

    case TOKEN_LINE:
      // linefeed는 escape하던지 안보여주던지...
      break;

    default:
      ret += " : ";
      ret += std::string(start, length);
      break;
  }

  return ret;
}
