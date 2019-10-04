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


// The maximum depth that interpolation can nest. For example, this string has
// three levels:
//
//      "outside %(one + "%(two + "%(three)")")"
#define MAX_INTERPOLATION_NESTING 8

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

struct Lexer {
  const char* source;
  const char* tokenStart;
  const char* currentChar;
  int currentLine;
  Token current;
  Token previous;

  int parens[MAX_INTERPOLATION_NESTING];
  int numParens;

  bool skip_new_lines;
  bool print_errors;
  bool has_errors;
};

struct Keyword {
  const char* identifier;
  size_t length;
  TokenType tokenType;
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

// Returns true if [c] is a valid (non-initial) identifier character.
bool IsName(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

// Returns true if [c] is a digit.
bool IsDigit(char c) { return c >= '0' && c <= '9'; }

// Returns the current character the parser is sitting on.
char PeekChar(Parser* parser) { return *parser->currentChar; }

// Returns the character after the current character.
char PeekNextChar(Parser* parser) {
  // If we're at the end of the source, don't read past it.
  if (PeekChar(parser) == '\0') {
    return '\0';
  }

  return *(parser->currentChar + 1);
}

// Advances the parser forward one character.
char NextChar(Parser* parser) {
  char c = PeekChar(parser);
  parser->currentChar++;
  if (c == '\n') {
    parser->currentLine++;
  }

  return c;
}

// If the current character is [c], consumes it and returns `true`.
bool MatchChar(Parser* parser, char c) {
  if (PeekChar(parser) != c) {
    return false;
  }

  NextChar(parser);
  return true;
}

// Sets the parser's current token to the given [type] and current character
// range.
void MakeToken(Parser* parser, TokenType type) {
  parser->current.type = type;
  parser->current.start = parser->tokenStart;
  parser->current.length = (int)(parser->currentChar - parser->tokenStart);
  parser->current.line = parser->currentLine;

  // Make line tokens appear on the line containing the "\n".
  if (type == TOKEN_LINE) {
    parser->current.line--;
  }
}

// If the current character is [c], then consumes it and makes a token of type
// [two]. Otherwise makes a token of type [one].
void TwoCharToken(Parser* parser, char c, TokenType two, TokenType one) {
  MakeToken(parser, MatchChar(parser, c) ? two : one);
}

// Skips the rest of the current line.
void SkipLineComment(Parser* parser) {
  while (PeekChar(parser) != '\n' && PeekChar(parser) != '\0') {
    NextChar(parser);
  }
}

// Skips the rest of a block comment.
void SkipBlockComment(Parser* parser) {
  int nesting = 1;
  while (nesting > 0) {
    if (PeekChar(parser) == '\0') {
      LexError(parser, "Unterminated block comment.");
      return;
    }

    if (PeekChar(parser) == '/' && PeekNextChar(parser) == '*') {
      NextChar(parser);
      NextChar(parser);
      nesting++;
      continue;
    }

    if (PeekChar(parser) == '*' && PeekNextChar(parser) == '/') {
      NextChar(parser);
      NextChar(parser);
      nesting--;
      continue;
    }

    // Regular comment characters.
    NextChar(parser);
  }
}

// Reads the next character, which should be a hex digit (0-9, a-f, or A-F) and
// returns its numeric value. If the character isn't a hex digit, returns -1.
int ReadHexDigit(Parser* parser) {
  char c = NextChar(parser);

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
  parser->currentChar--;
  return -1;
}

void MakeNumber(Parser* parser, bool hex) {
  errno = 0;

  if (hex) {
    parser->current.value =
        NUM_VAL((double)strtoll(parser->tokenStart, NULL, 16));
  } else {
    parser->current.value = NUM_VAL(strtod(parser->tokenStart, NULL));
  }

  if (errno == ERANGE) {
    LexError(parser, "Number literal was too large (%d).", sizeof(long int));
    parser->current.value = NUM_VAL(0);
  }

  // We don't check that the entire token is consumed after calling strtoll()
  // or strtod() because we've already scanned it ourselves and know it's valid.

  MakeToken(parser, TOKEN_NUMBER);
}

void ReadHexNumber(Parser* parser) {
  // Skip past the `x` used to denote a hexadecimal literal.
  NextChar(parser);

  // Iterate over all the valid hexadecimal digits found.
  while (ReadHexDigit(parser) != -1) {
    continue;
  }

  MakeNumber(parser, true);
}

void ReadNumber(Parser* parser) {
  if (IsDigit(PeekChar(parser)) {
    NextChar(parser);
  }

  // See if it has a floating point. Make sure there is a digit after the "."
  // so we don't get confused by method calls on number literals.
  if (PeekChar(parser) == '.' && IsDigit(PeekNextChar(parser))) {
    NextChar(parser);
    while (IsDigit(PeekChar(parser))) {
      NextChar(parser);
    }
  }

  // See if the number is in scientific notation.
  if (MatchChar(parser, 'e') || MatchChar(parser, 'E')) {
    // Allow a negative exponent.
    MatchChar(parser, '-');

    if (!IsDigit(PeekChar(parser))) {
      LexError(parser, "Unterminated scientific notation.");
    }

    while (IsDigit(PeekChar(parser))) {
      NextChar(parser);
    }
  }

  MakeNumber(parser, false);
}

// Finishes lexing an identifier. Handles reserved words.
void ReadName(Parser* parser, TokenType type) {
  while (IsName(PeekChar(parser)) || IsDigit(PeekChar(parser))) {
    NextChar(parser);
  }

  // Update the type if it's a keyword.
  size_t length = parser->currentChar - parser->tokenStart;
  for (int i = 0; keywords[i].identifier != nullptr; ++i) {
    if (length == keywords[i].length &&
        memcmp(parser->tokenStart, keywords[i].identifier, length) == 0) {
      type = keywords[i].tokenType;
      break;
    }
  }

  MakeToken(parser, type);
}

// Reads [digits] hex digits in a string literal and returns their number value.
int ReadHexEscape(Parser* parser, int digits, const char* description) {
  int value = 0;

  for (int i = 0; i < digits; ++i) {
    if (PeekChar(parser) == '"' || PeekChar(parser) == '\0') {
      LexError(parser, "Incomplete %s escape sequence.", description);

      // Don't consume it if it isn't expected. Keeps us from reading past the
      // end of an unterminated string.
      parser->currentChar--;
      break;
    }

    int digit = ReadHexDigit(parser);
    if (digit == -1) {
      LexError(parser, "Invalid %s escape sequence.", description);
      break;
    }

    value = (value * 16) | digit;
  }

  return value;
}

// Reads a hex digit Unicode escape sequence in a string literal.
void ReadUnicodeEscape(Parser* parser, ByteBuffer* string, int length) {
  int value = ReadHexEscape(parser, length, "Unicode");

  // Grow the buffer enough for the encoded result.
  int num_bytes = Utf8EncodeNumBytes(value);
  if (num_bytes > 0) {
    ByteBufferFill(parser->vm, string, 0, num_bytes);
    Utf8Encode(value, string->data + string->count - num_bytes);
  }
}

// Finishes lexing a string literal.
void ReadString(Parser* parser) {
  ByteBuffer string;
  TokenType type = TOKEN_STRING;
  ByteBufferInit(&string);

  for (;;) {
    char c = NextChar(parser);
    if (c == '"') {
      break;
    }

    if (c == '\0') {
      LexError(parser, "Unterminated string.");

      // Don't consume it if it isn't expected. Keeps us from reading past the
      // end of an unterminated string.
      parser->currentChar--;
      break;
    }

    if (c == '%') {
      if (parser->numParens < MAX_INTERPOLATION_NESTING) {
        // TODO: Allow format string.
        if (Next(parser) != '(') {
          LexError(parser, "Expect '(' after '%'.");
        }

        parser->parens[parser->numParens++] = 1;
        type = TOKEN_INTERPOLATION;
        break;
      }

      LexError(parse, "Interpolation may only nest %d levels deep.",
               MAX_INTERPOLATION_NESTING);
    }

    if (c == '\\') {
      char c2 = NextChar(parser);
      switch (c2) {
        case '"':
          ByteBufferWrite(parser->vm, &string, '"');
          break;
        case '\\':
          ByteBufferWrite(parser->vm, &string, '\\');
          break;
        case '%':
          ByteBufferWrite(parser->vm, &string, '%');
          break;
        case '0':
          ByteBufferWrite(parser->vm, &string, '\0');
          break;
        case 'a':
          ByteBufferWrite(parser->vm, &string, '\a');
          break;
        case 'b':
          ByteBufferWrite(parser->vm, &string, '\b');
          break;
        case 'f':
          ByteBufferWrite(parser->vm, &string, '\f');
          break;
        case 'n':
          ByteBufferWrite(parser->vm, &string, '\n');
          break;
        case 'r':
          ByteBufferWrite(parser->vm, &string, '\r');
          break;
        case 't':
          ByteBufferWrite(parser->vm, &string, '\t');
          break;
        case 'u':
          ReadUnicodeEscape(parser, &string, 4);
          break;
        case 'U':
          ReadUnicodeEscape(parser, &string, 8);
          break;
        case 'v':
          ByteBufferWrite(parser->vm, &string, '\v');
          break;
        case 'x':
          ByteBufferWrite(parser->vm, &string,
                          (uint8_t)ReadHexEscape(parser, 2, "byte"));
          break;

        default:
          LexError(parser, "Invalid escape character '%c'.",
                   *(parser->currentChar - 1));
          break;
      }
    } else {
      ByteBufferWrite(parser->vm, &string, c);
    }

    parser->current.value =
        NewStringLength(parser->vm, (char*)string.data, string.count);

    ByteBufferClear(parser->vm, &string);
    MakeToken(parser, type);
  }
}

static void NextToken(Parser* parser) {
  parser->previous = parser->current;

  if (parser->current.type == TOKEN_EOF) {
    return;
  }

  while (PeekChar(parser) != '\0') {
    parser->tokenStart = parser->currentChar;

    char c = NextChar(parser);
    switch (c) {
      case '(':
        if (parser->numParens > 0) {
          parser->parens[parser->numParens - 1]++;
        }
        MakeToken(parser, TOKEN_LEFT_PAREN);
        return;

      case ')':
        if (parser->numParens > 0 &&
            --parser->parens[parser->numParens - 1] == 0) {
          // This is the final ")", so the interpolation expression has ended.
          // This ")" now begins the next section of the template string.
          parser->numParens--;
          ReadString(parser);
          return;
        }

        MakeToken(parser, TOKEN_RIGHT_PAREN);
        return;

      case '[':
        MakeToken(parser, TOKEN_LEFT_BRACKET);
        return;
      case ']':
        MakeToken(parser, TOKEN_RIGHT_BRACKET);
        return;
      case '{':
        MakeToken(parser, TOKEN_LEFT_BRACE);
        return;
      case '}':
        MakeToken(parser, TOKEN_RIGHT_BRACE);
        return;
      case ':':
        MakeToken(parser, TOKEN_COLON);
        return;
      case ',':
        MakeToken(parser, TOKEN_COMMA);
        return;
      case '*':
        MakeToken(parser, TOKEN_STAR);
        return;
      case '%':
        MakeToken(parser, TOKEN_PERCENT);
        return;
      case '^':
        MakeToken(parser, TOKEN_CARET);
        return;
      case '+':
        MakeToken(parser, TOKEN_PLUS);
        return;
      case '-':
        MakeToken(parser, TOKEN_MINUS);
        return;
      case '~':
        MakeToken(parser, TOKEN_TILDE);
        return;
      case '?':
        MakeToken(parser, TOKEN_QUESTION);
        return;

      case '|':
        TwoCharToken(parser, '|', TOKEN_PIPEPIPE, TOKEN_PIPE);
        return;
      case '&':
        TwoCharToken(parser, '&', TOKEN_AMPAMP, TOKEN_AMP);
        return;
      case '=':
        TwoCharToken(parser, '=', TOKEN_EQEQ, TOKEN_EQ);
        return;
      case '!':
        TwoCharToken(parser, '=', TOKEN_BANGEQ, TOKEN_BANG);
        return;

      case '.':
        if (MatchChar(parser, '.')) {
          TwoCharToken(parser, '.', TOKEN_DOTDOTDOT, TOKEN_DOTDOT);
          return;
        }

        MakeToken(parser, TOKEN_DOT);
        return;

      case '/':
        if (MatchChar(parser, '/')) {
          SkipLineComment(parser);
          break;
        }

        if (MatchChar(parser, '*')) {
          SkipBlockComment(parser);
          break;
        }

        MakeToken(parser, TOKEN_SLASH);
        return;

      case '<':
        if (MatchChar(parser, '<')) {
          MakeToken(parser, TOKEN_LTLT);
        } else {
          TwoCharToken(parser, '=', TOKEN_LTEQ, TOKEN_LT);
        }
        return;

      case '>':
        if (MatchChar(parser, '>')) {
          MakeToken(parser, TOKEN_GTGT);
        } else {
          TwoCharToken(parser, '=', TOKEN_GTEQ, TOKEN_GT);
        }
        return;

      case '\n':
        MakeToken(parser, TOKEN_LINE);
        return;

      case ' ':
      case '\r':
      case '\t':
        // Skip forward until we run out of whitespace.
        while (PeekChar(parser) == ' ' || PeekChar(parser) == '\r' ||
               PeekChar(parser) == '\t') {
          NextChar(parser);
        }
        break;

      case '"':
        ReadString(parser);
        return;

      case '_':
        ReadName(parser,
                 PeekChar(parser) == '_' ? TOKEN_STATIC_FIELD : TOKEN_FIELD);
        return;

      case '0':
        if (PeekChar(parser) == 'x') {
          ReadHexNumber(parser);
          return;
        }

        ReadNumber(parser);
        return;

      default:
        if (parser->currentLine == 1 && c == '#' && PeekChar(parser) == '!') {
          // Ignore shebang on the first line.
          SkipLineComment(parser);
          break;
        }

        if (IsName(c)) {
          ReadName(parser, TOKEN_NAME);
        } else if (IsDigit(c)) {
          ReadNumber(parser);
        } else {
          if (c >= 32 && c <= 126) {
            LexError(parser, "Invalid character '%c'.", c);
          } else {
            LexError(parser, "Invalid byte 0x%x.", (uint8_t)c);
          }

          parser->current.type = TOKEN_ERROR;
          parser->current.length = 0;
        }

        return;
    }
  }

  // If we get here, we're out of source, so just make EOF tokens.
  parser->tokenStart = parser->currentChar;
  MakeToken(parser, TOKEN_EOF);
}

// Parsing --------------------------------------------------------------------

// Returns the type of the current token.
TokenType Peek(Compiler* compiler) {
  return compiler->parser->current.type;
}

// Consumes the current token if its type is [expected]. Returns true if a
// token was consumed.
bool Match(Compiler* compiler, TokenType expected) {
  if (Peek(compiler) != expected) {
    return false;
  }

  NextToken(compiler->parser);
  return true;
}

// Consumes the current token. Emits an error if its type is not [expected].
void Consume(Compiler* compiler, TokenType expected,
             const char* error_message) {
  NextToken(compiler->parser);

  if (compiler->parser->previous.type != expected) {
    Error(compiler, error_message);

    // If the next token is the one we want, assume the current one is just a
    // spurious error and discard it to minimize the number of cascaded errors.
    if (compiler->parser->current.type == expected) {
      NextToken(compiler->parser);
    }
  }
}

// Matches one or more newlines. Returns true if at least one was found.
bool MatchLine(Compiler* compiler) {
  if (!Match(compiler, TOKEN_LINE)) {
    return false;
  }

  while (Match(compiler, TOKEN_LINE))
    ;
  return true;
}

// Discards any newlines starting at the current token.
void IgnoreNewLines(Compiler* compiler) {
  MatchLine(compiler);
}

// Consumes the current token. Emits an error if it is not a newline. Then
// discards any duplicate newlines following it.
void ConsumeLine(Compiler* compiler, const char* error_message) {
  Consume(compiler, TOKEN_LINE, error_message);
  IgnoreNewLines(compiler);
}
