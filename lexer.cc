// {} : braces
// [] : brackets
// () : parenthesis

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

  int braces[MAX_INTERPOLATION_NESTING];
  int numBraces;

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
inline bool IsName(char c) {
  return (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || c == '_';
}

// Returns true if [c] is a digit.
inline bool IsDigit(char c) { return c >= '0' && c <= '9'; }

// Returns the current character the lexer is sitting on.
char Lexer::PeekChar(Lexer* lexer) { return *lexer->currentChar; }

// Returns the character after the current character.
char Lexer::PeekNextChar(Lexer* lexer) {
  // If we're at the end of the source, don't read past it.
  if (PeekChar(lexer) == '\0') {
    return '\0';
  }

  return *(lexer->currentChar + 1);
}

// Advances the lexer forward one character.
char Lexer::NextChar(Lexer* lexer) {
  char c = PeekChar(lexer);
  lexer->currentChar++;
  if (c == '\n') {
    lexer->currentLine++;
  }

  return c;
}

// If the current character is [c], consumes it and returns `true`.
bool Lexer::MatchChar(Lexer* lexer, char c) {
  if (PeekChar(lexer) != c) {
    return false;
  }

  NextChar(lexer);
  return true;
}

// Sets the lexer's current token to the given [type] and current character
// range.
void Lexer::MakeToken(Lexer* lexer, TokenType type) {
  lexer->current.type = type;
  lexer->current.start = lexer->tokenStart;
  lexer->current.length = (int)(lexer->currentChar - lexer->tokenStart);
  lexer->current.line = lexer->currentLine;

  // Make line tokens appear on the line containing the "\n".
  if (type == TOKEN_LINE) {
    lexer->current.line--;
  }
}

// If the current character is [c], then consumes it and makes a token of type
// [two]. Otherwise makes a token of type [one].
void Lexer::TwoCharToken(Lexer* lexer, char c, TokenType two, TokenType one) {
  MakeToken(lexer, MatchChar(lexer, c) ? two : one);
}

// Skips the rest of the current line.
void Lexer::SkipLineComment(Lexer* lexer) {
  while (PeekChar(lexer) != '\n' && PeekChar(lexer) != '\0') {
    NextChar(lexer);
  }
}

// Skips the rest of a block comment.
void Lexer::SkipBlockComment(Lexer* lexer) {
  int nesting = 1;
  while (nesting > 0) {
    if (PeekChar(lexer) == '\0') {
      LexError(lexer, "Unterminated block comment.");
      return;
    }

    if (PeekChar(lexer) == '/' && PeekNextChar(lexer) == '*') {
      NextChar(lexer);
      NextChar(lexer);
      nesting++;
      continue;
    }

    if (PeekChar(lexer) == '*' && PeekNextChar(lexer) == '/') {
      NextChar(lexer);
      NextChar(lexer);
      nesting--;
      continue;
    }

    // Regular comment characters.
    NextChar(lexer);
  }
}

// Reads the next character, which should be a hex digit (0-9, a-f, or A-F) and
// returns its numeric value. If the character isn't a hex digit, returns -1.
int Lexer::ReadHexDigit(Lexer* lexer) {
  char c = NextChar(lexer);

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
  lexer->currentChar--;
  return -1;
}

void Lexer::MakeNumber(Lexer* lexer, bool hex) {
  errno = 0;

  if (hex) {
    lexer->current.value =
        NUM_VAL((double)strtoll(lexer->tokenStart, NULL, 16));
  } else {
    lexer->current.value = NUM_VAL(strtod(lexer->tokenStart, NULL));
  }

  if (errno == ERANGE) {
    LexError(lexer, "Number literal was too large (%d).", sizeof(long int));
    lexer->current.value = NUM_VAL(0);
  }

  // We don't check that the entire token is consumed after calling strtoll()
  // or strtod() because we've already scanned it ourselves and know it's valid.

  MakeToken(lexer, TOKEN_NUMBER);
}

void Lexer::ReadHexNumber(Lexer* lexer) {
  // Skip past the `x` used to denote a hexadecimal literal.
  NextChar(lexer);

  // Iterate over all the valid hexadecimal digits found.
  while (ReadHexDigit(lexer) != -1) {
    continue;
  }

  MakeNumber(lexer, true);
}

void Lexer::ReadNumber(Lexer* lexer) {
  if (IsDigit(PeekChar(lexer)) {
    NextChar(lexer);
  }

  // See if it has a floating point. Make sure there is a digit after the "."
  // so we don't get confused by method calls on number literals.
  if (PeekChar(lexer) == '.' && IsDigit(PeekNextChar(lexer))) {
    NextChar(lexer);
    while (IsDigit(PeekChar(lexer))) {
      NextChar(lexer);
    }
  }

  // See if the number is in scientific notation.
  if (MatchChar(lexer, 'e') || MatchChar(lexer, 'E')) {
    // Allow a negative exponent.
    MatchChar(lexer, '-');

    if (!IsDigit(PeekChar(lexer))) {
      LexError(lexer, "Unterminated scientific notation.");
    }

    while (IsDigit(PeekChar(lexer))) {
      NextChar(lexer);
    }
  }

  MakeNumber(lexer, false);
}

// Finishes lexing an identifier. Handles reserved words.
void Lexer::ReadName(Lexer* lexer, TokenType type) {
  while (IsName(PeekChar(lexer)) || IsDigit(PeekChar(lexer))) {
    NextChar(lexer);
  }

  // Update the type if it's a keyword.
  size_t length = lexer->currentChar - lexer->tokenStart;
  for (int i = 0; keywords[i].identifier != nullptr; ++i) {
    if (length == keywords[i].length &&
        memcmp(lexer->tokenStart, keywords[i].identifier, length) == 0) {
      type = keywords[i].tokenType;
      break;
    }
  }

  MakeToken(lexer, type);
}

// Reads [digits] hex digits in a string literal and returns their number value.
int Lexer::ReadHexEscape(Lexer* lexer, int digits, const char* tag) {
  int value = 0;

  for (int i = 0; i < digits; ++i) {
    if (PeekChar(lexer) == '"' || PeekChar(lexer) == '\0') {
      LexError(lexer, "Incomplete %s escape sequence.", tag);

      // Don't consume it if it isn't expected. Keeps us from reading past the
      // end of an unterminated string.
      lexer->currentChar--;
      break;
    }

    int digit = ReadHexDigit(lexer);
    if (digit == -1) {
      LexError(lexer, "Invalid %s escape sequence.", tag);
      break;
    }

    value = (value * 16) | digit;
  }

  return value;
}

// Reads a hex digit Unicode escape sequence in a string literal.
void Lexer::ReadUnicodeEscape(Lexer* lexer, ByteBuffer* string, int length) {
  int value = ReadHexEscape(lexer, length, "Unicode");

  // Grow the buffer enough for the encoded result.
  int num_bytes = Utf8EncodeNumBytes(value);
  if (num_bytes > 0) {
    ByteBufferFill(lexer->vm, string, 0, num_bytes);
    Utf8Encode(value, string->data + string->count - num_bytes);
  }
}

// Finishes lexing a string literal.
void Lexer::ReadString(Lexer* lexer) {
  ByteBuffer string;
  TokenType type = TOKEN_STRING;
  ByteBufferInit(&string);

  for (;;) {
    char c = NextChar(lexer);
    if (c == '"') {
      break;
    }

    if (c == '\0') {
      LexError(lexer, "Unterminated string.");

      // Don't consume it if it isn't expected. Keeps us from reading past the
      // end of an unterminated string.
      lexer->currentChar--;
      break;
    }

    if (c == '$') {
      if (lexer->numBraces < MAX_INTERPOLATION_NESTING) {
        // TODO: Allow format string.
        // 어떻게 처리하지??

        //C#은 다음과 같음
        //   {<interpolationExpression>[,<alignment>][:<formatString>]}

        if (Next(lexer) != '{') {
          LexError(lexer, "Expect '{' after '%'.");
        }

        lexer->braces[lexer->numBraces++] = 1;
        type = TOKEN_INTERPOLATION;
        break;
      }

      LexError(parse, "Interpolation may only nest %d levels deep.",
               MAX_INTERPOLATION_NESTING);
    }

    if (c == '\\') {
      char c2 = NextChar(lexer);
      switch (c2) {
        case '"':
          ByteBufferWrite(lexer->vm, &string, '"');
          break;
        case '\\':
          ByteBufferWrite(lexer->vm, &string, '\\');
          break;
        case '%':
          ByteBufferWrite(lexer->vm, &string, '%');
          break;
        case '0':
          ByteBufferWrite(lexer->vm, &string, '\0');
          break;
        case 'a':
          ByteBufferWrite(lexer->vm, &string, '\a');
          break;
        case 'b':
          ByteBufferWrite(lexer->vm, &string, '\b');
          break;
        case 'f':
          ByteBufferWrite(lexer->vm, &string, '\f');
          break;
        case 'n':
          ByteBufferWrite(lexer->vm, &string, '\n');
          break;
        case 'r':
          ByteBufferWrite(lexer->vm, &string, '\r');
          break;
        case 't':
          ByteBufferWrite(lexer->vm, &string, '\t');
          break;
        case 'u':
          ReadUnicodeEscape(lexer, &string, 4);
          break;
        case 'U':
          ReadUnicodeEscape(lexer, &string, 8);
          break;
        case 'v':
          ByteBufferWrite(lexer->vm, &string, '\v');
          break;
        case 'x':
          ByteBufferWrite(lexer->vm, &string,
                          (uint8_t)ReadHexEscape(lexer, 2, "byte"));
          break;

        default:
          LexError(lexer, "Invalid escape character '%c'.",
                   *(lexer->currentChar - 1));
          break;
      }
    } else {
      ByteBufferWrite(lexer->vm, &string, c);
    }

    lexer->current.value =
        NewStringLength(lexer->vm, (char*)string.data, string.count);

    ByteBufferClear(lexer->vm, &string);
    MakeToken(lexer, type);
  }
}

void Lexer::NextToken(Lexer* lexer) {
  lexer->previous = lexer->current;

  if (lexer->current.type == TOKEN_EOF) {
    return;
  }

  while (PeekChar(lexer) != '\0') {
    lexer->tokenStart = lexer->currentChar;

    char c = NextChar(lexer);
    switch (c) {
      case '(':
        MakeToken(lexer, TOKEN_LEFT_PAREN);
        return;

      case ')':
        MakeToken(lexer, TOKEN_RIGHT_PAREN);
        return;

      case '[':
        MakeToken(lexer, TOKEN_LEFT_BRACKET);
        return;
      case ']':
        MakeToken(lexer, TOKEN_RIGHT_BRACKET);
        return;

      case '{':
        if (lexer->numBraces > 0) {
          lexer->braces[lexer->numBraces-1]++;
        }
        MakeToken(lexer, TOKEN_LEFT_BRACE);
        return;
      case '}':
        if (lexer->numBraces > 0 &&
            --lexer->braces[lexer->numBraces-1] == 0) {
          // This is the final ")", so the interpolation expression has ended.
          // This ")" now begins the next section of the template string.
          lexer->numBraces--;
          ReadString(lexer);
          return;
        }
        MakeToken(lexer, TOKEN_RIGHT_BRACE);
        return;

      case ':':
        MakeToken(lexer, TOKEN_COLON);
        return;
      case ',':
        MakeToken(lexer, TOKEN_COMMA);
        return;
      case '*':
        MakeToken(lexer, TOKEN_STAR);
        return;
      case '%':
        MakeToken(lexer, TOKEN_PERCENT);
        return;
      case '^':
        MakeToken(lexer, TOKEN_CARET);
        return;
      case '+':
        MakeToken(lexer, TOKEN_PLUS);
        return;
      case '-':
        MakeToken(lexer, TOKEN_MINUS);
        return;
      case '~':
        MakeToken(lexer, TOKEN_TILDE);
        return;
      case '?':
        MakeToken(lexer, TOKEN_QUESTION);
        return;

      case '|':
        TwoCharToken(lexer, '|', TOKEN_PIPEPIPE, TOKEN_PIPE);
        return;
      case '&':
        TwoCharToken(lexer, '&', TOKEN_AMPAMP, TOKEN_AMP);
        return;
      case '=':
        TwoCharToken(lexer, '=', TOKEN_EQEQ, TOKEN_EQ);
        return;
      case '!':
        TwoCharToken(lexer, '=', TOKEN_BANGEQ, TOKEN_BANG);
        return;

      case '.':
        if (MatchChar(lexer, '.')) {
          TwoCharToken(lexer, '.', TOKEN_DOTDOTDOT, TOKEN_DOTDOT);
          return;
        }

        MakeToken(lexer, TOKEN_DOT);
        return;

      case '/':
        if (MatchChar(lexer, '/')) {
          SkipLineComment(lexer);
          break;
        }

        if (MatchChar(lexer, '*')) {
          SkipBlockComment(lexer);
          break;
        }

        MakeToken(lexer, TOKEN_SLASH);
        return;

      case '<':
        if (MatchChar(lexer, '<')) {
          MakeToken(lexer, TOKEN_LTLT);
        } else {
          TwoCharToken(lexer, '=', TOKEN_LTEQ, TOKEN_LT);
        }
        return;

      case '>':
        if (MatchChar(lexer, '>')) {
          MakeToken(lexer, TOKEN_GTGT);
        } else {
          TwoCharToken(lexer, '=', TOKEN_GTEQ, TOKEN_GT);
        }
        return;

      case '\n':
        MakeToken(lexer, TOKEN_LINE);
        return;

      case ' ':
      case '\r':
      case '\t':
        // Skip forward until we run out of whitespace.
        while (PeekChar(lexer) == ' ' || PeekChar(lexer) == '\r' ||
               PeekChar(lexer) == '\t') {
          NextChar(lexer);
        }
        break;

      case '"':
        ReadString(lexer);
        return;

      case '_':
        ReadName(lexer,
                 PeekChar(lexer) == '_' ? TOKEN_STATIC_FIELD : TOKEN_FIELD);
        return;

      case '0':
        if (PeekChar(lexer) == 'x') {
          ReadHexNumber(lexer);
          return;
        }

        ReadNumber(lexer);
        return;

      default:
        if (lexer->currentLine == 1 && c == '#' && PeekChar(lexer) == '!') {
          // Ignore shebang on the first line.
          SkipLineComment(lexer);
          break;
        }

        if (IsName(c)) {
          ReadName(lexer, TOKEN_NAME);
        } else if (IsDigit(c)) {
          ReadNumber(lexer);
        } else {
          if (c >= 32 && c <= 126) {
            LexError(lexer, "Invalid character '%c'.", c);
          } else {
            LexError(lexer, "Invalid byte 0x%x.", (uint8_t)c);
          }

          lexer->current.type = TOKEN_ERROR;
          lexer->current.length = 0;
        }

        return;
    }
  }

  // If we get here, we're out of source, so just make EOF tokens.
  lexer->tokenStart = lexer->currentChar;
  MakeToken(lexer, TOKEN_EOF);
}



/*
// Parsing --------------------------------------------------------------------

// Returns the type of the current token.
TokenType Peek(Compiler* compiler) {
  return compiler->lexer->current.type;
}

// Consumes the current token if its type is [expected]. Returns true if a
// token was consumed.
bool Match(Compiler* compiler, TokenType expected) {
  if (Peek(compiler) != expected) {
    return false;
  }

  NextToken(compiler->lexer);
  return true;
}

// Consumes the current token. Emits an error if its type is not [expected].
void Consume(Compiler* compiler, TokenType expected,
             const char* error_message) {
  NextToken(compiler->lexer);

  if (compiler->lexer->previous.type != expected) {
    Error(compiler, error_message);

    // If the next token is the one we want, assume the current one is just a
    // spurious error and discard it to minimize the number of cascaded errors.
    if (compiler->lexer->current.type == expected) {
      NextToken(compiler->lexer);
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
*/



void Lexer::Init(const char* source) {
  this->source = source;
  this->tokenStart = source;
  this->currentChar = source;
  this->currentLine = 1;
  this->numBraces = 0;

  this->current.type = TOKEN_ERROR;
  this->current.start = source;
  this->current.length = 0;
  this->current.line = UNDEFINED_VAL;

  // Ignore leading newlines.
  this->skip_new_lines = true;
  this->print_errors = true;

  // Read the first token.
  NextToken();
}






//TODO: 내부적으로 string builder를 지원해야 효율적으로 지원이 가능해보임.
void StringInterpolation(Compiler* compiler, bool can_assign) {
  LoadCoreVariable(compiler, "List");
  CallMethod(compiler, 0, "new()", 5);

  do {
    // The opening starting part.
    Literal(compiler, false);
    CallMethod(compiler, 1, "addCore_(_)", 11);

    // The interpolated expression.
    IgnoreNewLines(compiler);
    Expression(compression);
    CallMethod(compiler, 1, "addCore_(_)", 11);

    IgnoreNewLines(compiler);
  } while (Match(compiler, TOKEN_INTERPOLATION));

  // The trailing string part.
  Consume(compiler, TOKEN_STRING, "Expect end of string interpolation.");
  Literal(compiler, false);
  CallMethod(compiler, 1, "addCore_(_)", 11);

  // The list of interpolated parts.
  callMethod(compiler, 0, "join()", 6);
}
