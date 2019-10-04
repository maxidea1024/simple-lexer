#include "lexer.h"
#include <stdint.h>
#include <string.h>
#include <stdlib.h>

/*
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
*/

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

Lexer::Lexer(const char* source, int source_length) {
  Init(source, source_length);
}

void Lexer::Init(const char* source, int source_length) {
  source_ = source;
  token_start_ = source;
  current_char_ = source;
  current_line_ = 1;
  num_braces_ = 0;

  current.type = TOKEN_ERROR;
  current.start = source;
  current.length = 0;
  current.line = UNDEFINED_VAL;

  // Ignore leading newlines.
  skip_new_lines_ = true;
  print_errors_ = true;

  // Read the first token.
  NextToken();
}

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
  current_char_++;
  if (c == '\n') {
    current_line_++;
  }

  return c;
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
  current_char_--;
  return -1;
}

void Lexer::MakeNumber(bool hex) {
  errno = 0;

  if (hex) {
    current.value = NUM_VAL((double)strtoll(token_start_, NULL, 16));
  } else {
    current.value = NUM_VAL(strtod(token_start_, NULL));
  }

  if (errno == ERANGE) {
    LexError("Number literal was too large (%d).", sizeof(long int));
    current.value = NUM_VAL(0);
  }

  // We don't check that the entire token is consumed after calling strtoll()
  // or strtod() because we've already scanned it ourselves and know it's valid.

  MakeToken(TOKEN_NUMBER);
}

void Lexer::ReadHexNumber() {
  // Skip past the `x` used to denote a hexadecimal literal.
  NextChar();

  // Iterate over all the valid hexadecimal digits found.
  while (ReadHexDigit() != -1) {
    continue;
  }

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
      current_char_--;
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
void Lexer::ReadUnicodeEscape(ByteBuffer* string, int length) {
  int value = ReadHexEscape(length, "Unicode");

  // Grow the buffer enough for the encoded result.
  int num_bytes = Utf8EncodeNumBytes(value);
  if (num_bytes > 0) {
    ByteBufferFill(vm, string, 0, num_bytes);
    Utf8Encode(value, string->data + string->count - num_bytes);
  }
}

// Finishes lexing a string literal.
void Lexer::ReadString() {
  ByteBuffer string;
  TokenType type = TOKEN_STRING;
  ByteBufferInit(&string);

  for (;;) {
    char c = NextChar();
    if (c == '"') {
      break;
    }

    if (c == '\0') {
      LexError("Unterminated string.");

      // Don't consume it if it isn't expected. Keeps us from reading past the
      // end of an unterminated string.
      current_char_--;
      break;
    }

    if (c == '$') {
      if (num_braces_ < MAX_INTERPOLATION_NESTING) {
        // TODO: Allow format string.
        // 어떻게 처리하지??

        // C#은 다음과 같음
        //   {<interpolationExpression>[,<alignment>][:<formatString>]}

        if (NextChar() != '{') {
          LexError("Expect '{' after '%'.");
        }

        braces_[num_braces_++] = 1;
        type = TOKEN_INTERPOLATION;
        break;
      }

      LexError(parse, "Interpolation may only nest %d levels deep.",
               MAX_INTERPOLATION_NESTING);
    }

    if (c == '\\') {
      char c2 = NextChar();
      switch (c2) {
        case '"':
          ByteBufferWrite(vm, &string, '"');
          break;
        case '\\':
          ByteBufferWrite(vm, &string, '\\');
          break;
        case '%':
          ByteBufferWrite(vm, &string, '%');
          break;
        case '0':
          ByteBufferWrite(vm, &string, '\0');
          break;
        case 'a':
          ByteBufferWrite(vm, &string, '\a');
          break;
        case 'b':
          ByteBufferWrite(vm, &string, '\b');
          break;
        case 'f':
          ByteBufferWrite(vm, &string, '\f');
          break;
        case 'n':
          ByteBufferWrite(vm, &string, '\n');
          break;
        case 'r':
          ByteBufferWrite(vm, &string, '\r');
          break;
        case 't':
          ByteBufferWrite(vm, &string, '\t');
          break;
        case 'u':
          ReadUnicodeEscape(&string, 4);
          break;
        case 'U':
          ReadUnicodeEscape(&string, 8);
          break;
        case 'v':
          ByteBufferWrite(vm, &string, '\v');
          break;
        case 'x':
          ByteBufferWrite(vm, &string, (uint8_t)ReadHexEscape(2, "byte"));
          break;

        default:
          LexError("Invalid escape character '%c'.", *(current_char_ - 1));
          break;
      }
    } else {
      ByteBufferWrite(vm, &string, c);
    }

    current.value = NewStringLength(vm, (char*)string.data, string.count);

    ByteBufferClear(vm, &string);
    MakeToken(type);
  }
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
        if (num_braces_ > 0) {
          braces_[num_braces_ - 1]++;
        }
        MakeToken(TOKEN_LEFT_BRACE);
        return;
      case '}':
        if (num_braces_ > 0 && --braces_[num_braces_ - 1] == 0) {
          // This is the final ")", so the interpolation expression has ended.
          // This ")" now begins the next section of the template string.
          num_braces_--;
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
        MakeToken(TOKEN_STAR);
        return;
      case '%':
        MakeToken(TOKEN_PERCENT);
        return;
      case '^':
        MakeToken(TOKEN_CARET);
        return;
      case '+':
        MakeToken(TOKEN_PLUS);
        return;
      case '-':
        MakeToken(TOKEN_MINUS);
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
          TwoCharToken('.', TOKEN_DOTDOTDOT, TOKEN_DOTDOT);
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
        ReadString();
        return;

      case '_':
        ReadName(PeekChar() == '_' ? TOKEN_STATIC_FIELD : TOKEN_FIELD);
        return;

      case '0':
        if (PeekChar() == 'x') {
          ReadHexNumber();
          return;
        }

        ReadNumber();
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
  // TODO
}
