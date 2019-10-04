
static void printError(Parser* parser, int line, const char* label,
                       const char* format, va_list args) {
  parser->hasError = true;
  if (!parser->printErrors) return;

  // Only report errors if there is a WrenErrorFn to handle them.
  if (parser->vm->config.errorFn == NULL) return;

  // Format the label and message.
  char message[ERROR_MESSAGE_SIZE];
  int length = sprintf(message, "%s: ", label);
  length += vsprintf(message + length, format, args);
  ASSERT(length < ERROR_MESSAGE_SIZE, "Error should not exceed buffer.");

  ObjString* module = parser->module->name;
  const char* module_name = module ? module->value : "<unknown>";

  parser->vm->config.errorFn(parser->vm, WREN_ERROR_COMPILE, module_name, line,
                             message);
}

// Outputs a lexical error.
static void lexError(Parser* parser, const char* format, ...) {
  va_list args;
  va_start(args, format);
  printError(parser, parser->currentLine, "Error", format, args);
  va_end(args);
}

// Outputs a compile or syntax error. This also marks the compilation as having
// an error, which ensures that the resulting code will be discarded and never
// run. This means that after calling error(), it's fine to generate whatever
// invalid bytecode you want since it won't be used.
//
// You'll note that most places that call error() continue to parse and compile
// after that. That's so that we can try to find as many compilation errors in
// one pass as possible instead of just bailing at the first one.
static void error(Compiler* compiler, const char* format, ...) {
  Token* token = &compiler->parser->previous;

  // If the parse error was caused by an error token, the lexer has already
  // reported it.
  if (token->type == TOKEN_ERROR) return;

  va_list args;
  va_start(args, format);
  if (token->type == TOKEN_LINE) {
    printError(compiler->parser, token->line, "Error at newline", format, args);
  } else if (token->type == TOKEN_EOF) {
    printError(compiler->parser, token->line, "Error at end of file", format,
               args);
  } else {
    // Make sure we don't exceed the buffer with a very long token.
    char label[10 + MAX_VARIABLE_NAME + 4 + 1];
    if (token->length <= MAX_VARIABLE_NAME) {
      sprintf(label, "Error at '%.*s'", token->length, token->start);
    } else {
      sprintf(label, "Error at '%.*s...'", MAX_VARIABLE_NAME, token->start);
    }
    printError(compiler->parser, token->line, label, format, args);
  }
  va_end(args);
}
