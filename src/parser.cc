
TokenType Parser::PeekToken() {
  return lexer_.current.type;
}

bool Parser::Match(TokenType expected) {
  if (PeekToken() != expected) {
    return false;
  }

  NextToken();
  return true;
}

void Parser::Expect(TokenType expected, const char* error_message) {
  NextToken();

  if (lexer_.previous.token != expected) {
    Error(error_message);

    if (lexer_.current.type == expected) {
      NextToken();
    }
  }
}

bool Parser::MatchLine() {
  if (!Match(TOKEN_LINE)) {
    return false;
  }

  while (Match(TOKEN_LINE));
  return true;
}

void Parser::IgnoreNewLines() {
  MatchLine();
}

void Parser::ExpectLine(const char* error_message) {
  Expect(TOKEN_LINE, error_message);
  IgnoreNewLines();
}


void Parser::PushScope() {
  scope_depth_++;
}

void Parser::PopScope() {
  //TODO something...

  scope_depth_--;
}


// Grammar ---------------------------------------------------------------------

typedef enum
{
  PREC_NONE,
  PREC_LOWEST,
  PREC_ASSIGNMENT,    // =
  PREC_CONDITIONAL,   // ?:
  PREC_LOGICAL_OR,    // ||
  PREC_LOGICAL_AND,   // &&
  PREC_EQUALITY,      // == !=
  PREC_IS,            // is
  PREC_COMPARISON,    // < > <= >=
  PREC_BITWISE_OR,    // |
  PREC_BITWISE_XOR,   // ^
  PREC_BITWISE_AND,   // &
  PREC_BITWISE_SHIFT, // << >>
  PREC_RANGE,         // .. ...
  PREC_TERM,          // + -
  PREC_FACTOR,        // * / %
  PREC_UNARY,         // unary - ! ~
  PREC_CALL,          // . () []
  PREC_PRIMARY
} Precedence;

typedef void (*GrammarFn)(Compiler*, bool canAssign);

typedef void (*SignatureFn)(Signature* signature);

typedef struct
{
  GrammarFn prefix;
  GrammarFn infix;
  SignatureFn method;
  Precedence precedence;
  const char* name;
} GrammarRule;

// Forward declarations since the grammar is recursive.
static GrammarRule* getRule(TokenType type);
static void expression();
static void statement();
static void definition();
static void parsePrecedence(Precedence precedence);

static void PatchJump(int offset)
{
  // -2 to adjust for the bytecode for the jump offset itself.
  int jump = fn_->code.count - offset - 2;
  if (jump > MAX_JUMP) {
    Error("Too much code to jump over.");
  }

  // 2byte offset
  fn_->code.data[offset] = (jump >> 8) & 0xff;
  fn_->code.data[offset + 1] = jump & 0xff;
}

static bool FinishBlock()
{
  // Empty blocks do nothing.
  if (Match(TOKEN_RIGHT_BRACE)) {
    return false;
  }

  // If there's no line after the "{", it's a single-expression body.
  if (!MatchLine(compiler)) {
    Expression(compiler);
    Expect(TOKEN_RIGHT_BRACE, "Expect '}' at end of block.");
    return true;
  }

  // Empty blocks (with just a newline inside) do nothing.
  if (Match(TOKEN_RIGHT_BRACE)) return false;

  // Compile the definition list.
  do {
    Definition(compiler);
    ConsumeLine(compiler, "Expect newline after statement.");
  } while (Peek(compiler) != TOKEN_RIGHT_BRACE && Peek(compiler) != TOKEN_EOF);

  Expect(TOKEN_RIGHT_BRACE, "Expect '}' at end of block.");
  return false;
}

static void FinishBody(bool isInitializer)
{
  bool isExpressionBody = FinishBlock(compiler);

  if (isInitializer)
  {
    // If the initializer body evaluates to a value, discard it.
    if (isExpressionBody) EmitOp(compiler, CODE_POP);

    // The receiver is always stored in the first local slot.
    EmitOp(compiler, CODE_LOAD_LOCAL_0);
  }
  else if (!isExpressionBody)
  {
    // Implicitly return null in statement bodies.
    EmitOp(compiler, CODE_NULL);
  }

  EmitOp(compiler, CODE_RETURN);
}

static void ValidateNumParameters(int numArgs)
{
  if (numArgs == MAX_PARAMETERS + 1)
  {
    // Only show an error at exactly max + 1 so that we can keep parsing the
    // parameters and minimize cascaded errors.
    Error("Methods cannot have more than %d parameters.",
          MAX_PARAMETERS);
  }
}

static void finishParameterList(Signature* signature)
{
  do
  {
    ignoreNewlines(compiler);
    ValidateNumParameters(compiler, ++signature->arity);

    // Define a local variable in the method for the parameter.
    declareNamedVariable(compiler);
  }
  while (Match(TOKEN_COMMA));
}

// Gets the symbol for a method [name] with [length].
static int methodSymbol(const char* name, int length)
{
  return wrenSymbolTableEnsure(compiler->parser->vm,
      &compiler->parser->vm->methodNames, name, length);
}

static void signatureParameterList(char name[MAX_METHOD_SIGNATURE], int* length,
                                   int numParams, char leftBracket, char rightBracket)
{
  name[(*length)++] = leftBracket;

  // This function may be called with too many parameters. When that happens,
  // a compile error has already been reported, but we need to make sure we
  // don't overflow the string too, hence the MAX_PARAMETERS check.
  for (int i = 0; i < numParams && i < MAX_PARAMETERS; i++)
  {
    if (i > 0) name[(*length)++] = ',';
    name[(*length)++] = '_';
  }
  name[(*length)++] = rightBracket;
}

static void signatureToString(Signature* signature,
                              char name[MAX_METHOD_SIGNATURE], int* length)
{
  *length = 0;

  // Build the full name from the signature.
  memcpy(name + *length, signature->name, signature->length);
  *length += signature->length;

  switch (signature->type)
  {
    case SIG_METHOD:
      signatureParameterList(name, length, signature->arity, '(', ')');
      break;

    case SIG_GETTER:
      // The signature is just the name.
      break;

    case SIG_SETTER:
      name[(*length)++] = '=';
      signatureParameterList(name, length, 1, '(', ')');
      break;

    case SIG_SUBSCRIPT:
      signatureParameterList(name, length, signature->arity, '[', ']');
      break;

    case SIG_SUBSCRIPT_SETTER:
      signatureParameterList(name, length, signature->arity - 1, '[', ']');
      name[(*length)++] = '=';
      signatureParameterList(name, length, 1, '(', ')');
      break;

    case SIG_INITIALIZER:
      memcpy(name, "init ", 5);
      memcpy(name + 5, signature->name, signature->length);
      *length = 5 + signature->length;
      signatureParameterList(name, length, signature->arity, '(', ')');
      break;
  }

  name[*length] = '\0';
}

static int signatureSymbol(Signature* signature)
{
  // Build the full name from the signature.
  char name[MAX_METHOD_SIGNATURE];
  int length;
  signatureToString(signature, name, &length);

  return methodSymbol(compiler, name, length);
}

static Signature signatureFromToken(SignatureType type)
{
  Signature signature;

  // Get the token for the method name.
  Token* token = &compiler->parser->previous;
  signature.name = token->start;
  signature.length = token->length;
  signature.type = type;
  signature.arity = 0;

  if (signature.length > MAX_METHOD_NAME)
  {
    Error("Method names cannot be longer than %d characters.",
          MAX_METHOD_NAME);
    signature.length = MAX_METHOD_NAME;
  }

  return signature;
}

static void FinishArgumentList(Signature* signature) {
  do {
    ignoreNewlines(compiler);
    ValidateNumParameters(compiler, ++signature->arity);
    expression(compiler);
  } while (Match(TOKEN_COMMA));

  // Allow a newline before the closing delimiter.
  ignoreNewlines(compiler);
}

static void CallSignature(Code instruction,
                          Signature* signature)
{
  int symbol = signatureSymbol(compiler, signature);
  emitShortArg(compiler, (Code)(instruction + signature->arity), symbol);

  if (instruction == CODE_SUPER_0)
  {
    // Super calls need to be statically bound to the class's superclass. This
    // ensures we call the right method even when a method containing a super
    // call is inherited by another subclass.
    //
    // We bind it at class definition time by storing a reference to the
    // superclass in a constant. So, here, we create a slot in the constant
    // table and store NULL in it. When the method is bound, we'll look up the
    // superclass then and store it in the constant slot.
    emitShort(compiler, addConstant(compiler, NULL_VAL));
  }
}

static void callMethod(int numArgs, const char* name,
                       int length)
{
  int symbol = methodSymbol(compiler, name, length);
  emitShortArg(compiler, (Code)(CODE_CALL_0 + numArgs), symbol);
}

static void methodCall(Code instruction,
                       Signature* signature)
{
  // Make a new signature that contains the updated arity and type based on
  // the arguments we find.
  Signature called = { signature->name, signature->length, SIG_GETTER, 0 };

  // Parse the argument list, if any.
  if (Match(TOKEN_LEFT_PAREN))
  {
    called.type = SIG_METHOD;

    // Allow empty an argument list.
    if (Peek(compiler) != TOKEN_RIGHT_PAREN)
    {
      FinishArgumentList(compiler, &called);
    }
    Expect(TOKEN_RIGHT_PAREN, "Expect ')' after arguments.");
  }

  // Parse the block argument, if any.
  if (Match(TOKEN_LEFT_BRACE))
  {
    // Include the block argument in the arity.
    called.type = SIG_METHOD;
    called.arity++;

    Compiler fnCompiler;
    initCompiler(&fnCompiler, compiler->parser, compiler, false);

    // Make a dummy signature to track the arity.
    Signature fnSignature = { "", 0, SIG_METHOD, 0 };

    // Parse the parameter list, if any.
    if (Match(TOKEN_PIPE))
    {
      finishParameterList(&fnCompiler, &fnSignature);
      Expect(TOKEN_PIPE, "Expect '|' after function parameters.");
    }

    fnCompiler.fn->arity = fnSignature.arity;

    FinishBody(&fnCompiler, false);

    // Name the function based on the method its passed to.
    char blockName[MAX_METHOD_SIGNATURE + 15];
    int blockLength;
    signatureToString(&called, blockName, &blockLength);
    memmove(blockName + blockLength, " block argument", 16);

    endCompiler(&fnCompiler, blockName, blockLength + 15);
  }

  // TODO: Allow Grace-style mixfix methods?

  // If this is a super() call for an initializer, make sure we got an actual
  // argument list.
  if (signature->type == SIG_INITIALIZER)
  {
    if (called.type != SIG_METHOD)
    {
      Error("A superclass constructor must have an argument list.");
    }

    called.type = SIG_INITIALIZER;
  }

  CallSignature(compiler, instruction, &called);
}

static void namedCall(bool canAssign, Code instruction)
{
  // Get the token for the method name.
  Signature signature = signatureFromToken(compiler, SIG_GETTER);

  if (canAssign && Match(TOKEN_EQ))
  {
    ignoreNewlines(compiler);

    // Build the setter signature.
    signature.type = SIG_SETTER;
    signature.arity = 1;

    // Compile the assigned value.
    expression(compiler);
    CallSignature(compiler, instruction, &signature);
  }
  else
  {
    methodCall(compiler, instruction, &signature);
  }
}

static void loadVariable(Variable variable)
{
  switch (variable.scope)
  {
    case SCOPE_LOCAL:
      loadLocal(compiler, variable.index);
      break;
    case SCOPE_UPVALUE:
      emitByteArg(compiler, CODE_LOAD_UPVALUE, variable.index);
      break;
    case SCOPE_MODULE:
      emitShortArg(compiler, CODE_LOAD_MODULE_VAR, variable.index);
      break;
    default:
      UNREACHABLE();
  }
}

static void loadThis()
{
  loadVariable(compiler, resolveNonmodule(compiler, "this", 4));
}

static void loadCoreVariable(const char* name)
{
  int symbol = wrenSymbolTableFind(&compiler->parser->module->variableNames,
                                   name, strlen(name));
  ASSERT(symbol != -1, "Should have already defined core name.");
  emitShortArg(compiler, CODE_LOAD_MODULE_VAR, symbol);
}

// A parenthesized expression.
static void grouping(bool canAssign)
{
  expression(compiler);
  Expect(TOKEN_RIGHT_PAREN, "Expect ')' after expression.");
}

static void list(bool canAssign)
{
  // Instantiate a new list.
  loadCoreVariable(compiler, "List");
  callMethod(compiler, 0, "new()", 5);

  // Compile the list elements. Each one compiles to a ".add()" call.
  do
  {
    ignoreNewlines(compiler);

    // Stop if we hit the end of the list.
    if (Peek(compiler) == TOKEN_RIGHT_BRACKET) break;

    // The element.
    expression(compiler);
    callMethod(compiler, 1, "addCore_(_)", 11);
  } while (Match(TOKEN_COMMA));

  // Allow newlines before the closing ']'.
  ignoreNewlines(compiler);
  Expect(TOKEN_RIGHT_BRACKET, "Expect ']' after list elements.");
}

static void map(bool canAssign)
{
  // Instantiate a new map.
  loadCoreVariable(compiler, "Map");
  callMethod(compiler, 0, "new()", 5);

  // Compile the map elements. Each one is compiled to just invoke the
  // subscript setter on the map.
  do
  {
    ignoreNewlines(compiler);

    // Stop if we hit the end of the map.
    if (Peek(compiler) == TOKEN_RIGHT_BRACE) break;

    // The key.
    parsePrecedence(compiler, PREC_UNARY);
    Expect(TOKEN_COLON, "Expect ':' after map key.");
    ignoreNewlines(compiler);

    // The value.
    expression(compiler);
    callMethod(compiler, 2, "addCore_(_,_)", 13);
  } while (Match(TOKEN_COMMA));

  // Allow newlines before the closing '}'.
  ignoreNewlines(compiler);
  Expect(TOKEN_RIGHT_BRACE, "Expect '}' after map entries.");
}

static void unaryOp(bool canAssign)
{
  GrammarRule* rule = getRule(compiler->parser->previous.type);

  ignoreNewlines(compiler);

  // Compile the argument.
  parsePrecedence(compiler, (Precedence)(PREC_UNARY + 1));

  // Call the operator method on the left-hand side.
  callMethod(compiler, 0, rule->name, 1);
}

static void boolean(bool canAssign)
{
  EmitOp(compiler,
      compiler->parser->previous.type == TOKEN_FALSE ? CODE_FALSE : CODE_TRUE);
}

static Compiler* getEnclosingClassCompiler()
{
  while (compiler != NULL)
  {
    if (compiler->enclosingClass != NULL) return compiler;
    compiler = compiler->parent;
  }

  return NULL;
}

static ClassInfo* getEnclosingClass()
{
  compiler = getEnclosingClassCompiler(compiler);
  return compiler == NULL ? NULL : compiler->enclosingClass;
}

static void field(bool canAssign)
{
  // Initialize it with a fake value so we can keep parsing and minimize the
  // number of cascaded errors.
  int field = 255;

  ClassInfo* enclosingClass = getEnclosingClass(compiler);

  if (enclosingClass == NULL)
  {
    Error("Cannot reference a field outside of a class definition.");
  }
  else if (enclosingClass->isForeign)
  {
    Error("Cannot define fields in a foreign class.");
  }
  else if (enclosingClass->inStatic)
  {
    Error("Cannot use an instance field in a static method.");
  }
  else
  {
    // Look up the field, or implicitly define it.
    field = wrenSymbolTableEnsure(compiler->parser->vm, &enclosingClass->fields,
        compiler->parser->previous.start,
        compiler->parser->previous.length);

    if (field >= MAX_FIELDS)
    {
      Error("A class can only have %d fields.", MAX_FIELDS);
    }
  }

  // If there's an "=" after a field name, it's an assignment.
  bool isLoad = true;
  if (canAssign && Match(TOKEN_EQ))
  {
    // Compile the right-hand side.
    expression(compiler);
    isLoad = false;
  }

  // If we're directly inside a method, use a more optimal instruction.
  if (compiler->parent != NULL &&
      compiler->parent->enclosingClass == enclosingClass)
  {
    emitByteArg(compiler, isLoad ? CODE_LOAD_FIELD_THIS : CODE_STORE_FIELD_THIS,
                field);
  }
  else
  {
    loadThis(compiler);
    emitByteArg(compiler, isLoad ? CODE_LOAD_FIELD : CODE_STORE_FIELD, field);
  }
}

static void bareName(bool canAssign, Variable variable)
{
  // If there's an "=" after a bare name, it's a variable assignment.
  if (canAssign && Match(TOKEN_EQ))
  {
    // Compile the right-hand side.
    expression(compiler);

    // Emit the store instruction.
    switch (variable.scope)
    {
      case SCOPE_LOCAL:
        emitByteArg(compiler, CODE_STORE_LOCAL, variable.index);
        break;
      case SCOPE_UPVALUE:
        emitByteArg(compiler, CODE_STORE_UPVALUE, variable.index);
        break;
      case SCOPE_MODULE:
        emitShortArg(compiler, CODE_STORE_MODULE_VAR, variable.index);
        break;
      default:
        UNREACHABLE();
    }
    return;
  }

  // Emit the load instruction.
  loadVariable(compiler, variable);
}

static void staticField(bool canAssign)
{
  Compiler* classCompiler = getEnclosingClassCompiler(compiler);
  if (classCompiler == NULL)
  {
    Error("Cannot use a static field outside of a class definition.");
    return;
  }

  // Look up the name in the scope chain.
  Token* token = &compiler->parser->previous;

  // If this is the first time we've seen this static field, implicitly
  // define it as a variable in the scope surrounding the class definition.
  if (resolveLocal(classCompiler, token->start, token->length) == -1)
  {
    int symbol = declareVariable(classCompiler, NULL);

    // Implicitly initialize it to null.
    EmitOp(classCompiler, CODE_NULL);
    defineVariable(classCompiler, symbol);
  }

  // It definitely exists now, so resolve it properly. This is different from
  // the above resolveLocal() call because we may have already closed over it
  // as an upvalue.
  Variable variable = resolveName(compiler, token->start, token->length);
  bareName(compiler, canAssign, variable);
}

static bool isLocalName(const char* name)
{
  return name[0] >= 'a' && name[0] <= 'z';
}

static void name(bool canAssign)
{
  // Look for the name in the scope chain up to the nearest enclosing method.
  Token* token = &compiler->parser->previous;

  Variable variable = resolveNonmodule(compiler, token->start, token->length);
  if (variable.index != -1)
  {
    bareName(compiler, canAssign, variable);
    return;
  }

  // TODO: The fact that we return above here if the variable is known and parse
  // an optional argument list below if not means that the grammar is not
  // context-free. A line of code in a method like "someName(foo)" is a parse
  // error if "someName" is a defined variable in the surrounding scope and not
  // if it isn't. Fix this. One option is to have "someName(foo)" always
  // resolve to a self-call if there is an argument list, but that makes
  // getters a little confusing.

  // If we're inside a method and the name is lowercase, treat it as a method
  // on this.
  if (isLocalName(token->start) && getEnclosingClass(compiler) != NULL)
  {
    loadThis(compiler);
    namedCall(compiler, canAssign, CODE_CALL_0);
    return;
  }

  // Otherwise, look for a module-level variable with the name.
  variable.scope = SCOPE_MODULE;
  variable.index = wrenSymbolTableFind(&compiler->parser->module->variableNames,
                                       token->start, token->length);
  if (variable.index == -1)
  {
    // Implicitly define a module-level variable in
    // the hopes that we get a real definition later.
    variable.index = wrenDeclareVariable(compiler->parser->vm,
                                         compiler->parser->module,
                                         token->start, token->length,
                                         token->line);

    if (variable.index == -2)
    {
      Error("Too many module variables defined.");
    }
  }

  bareName(compiler, canAssign, variable);
}

static void null(bool canAssign)
{
  EmitOp(compiler, CODE_NULL);
}

// A number or string literal.
static void literal(bool canAssign)
{
  emitConstant(compiler, compiler->parser->previous.value);
}

static void stringInterpolation(bool canAssign)
{
  // Instantiate a new list.
  loadCoreVariable(compiler, "List");
  callMethod(compiler, 0, "new()", 5);

  do
  {
    // The opening string part.
    literal(compiler, false);
    callMethod(compiler, 1, "addCore_(_)", 11);

    // The interpolated expression.
    ignoreNewlines(compiler);
    expression(compiler);
    callMethod(compiler, 1, "addCore_(_)", 11);

    ignoreNewlines(compiler);
  } while (Match(TOKEN_INTERPOLATION));

  // The trailing string part.
  Expect(TOKEN_STRING, "Expect end of string interpolation.");
  literal(compiler, false);
  callMethod(compiler, 1, "addCore_(_)", 11);

  // The list of interpolated parts.
  callMethod(compiler, 0, "join()", 6);
}

static void super_(bool canAssign)
{
  ClassInfo* enclosingClass = getEnclosingClass(compiler);
  if (enclosingClass == NULL)
  {
    Error("Cannot use 'super' outside of a method.");
  }

  loadThis(compiler);

  // TODO: Super operator calls.
  // TODO: There's no syntax for invoking a superclass constructor with a
  // different name from the enclosing one. Figure that out.

  // See if it's a named super call, or an unnamed one.
  if (Match(TOKEN_DOT))
  {
    // Compile the superclass call.
    Expect(TOKEN_NAME, "Expect method name after 'super.'.");
    namedCall(compiler, canAssign, CODE_SUPER_0);
  }
  else if (enclosingClass != NULL)
  {
    // No explicit name, so use the name of the enclosing method. Make sure we
    // check that enclosingClass isn't NULL first. We've already reported the
    // error, but we don't want to crash here.
    methodCall(compiler, CODE_SUPER_0, enclosingClass->signature);
  }
}

static void this_(bool canAssign)
{
  if (getEnclosingClass(compiler) == NULL)
  {
    Error("Cannot use 'this' outside of a method.");
    return;
  }

  loadThis(compiler);
}

static void subscript(bool canAssign)
{
  Signature signature = { "", 0, SIG_SUBSCRIPT, 0 };

  // Parse the argument list.
  FinishArgumentList(compiler, &signature);
  Expect(TOKEN_RIGHT_BRACKET, "Expect ']' after arguments.");

  if (canAssign && Match(TOKEN_EQ))
  {
    signature.type = SIG_SUBSCRIPT_SETTER;

    // Compile the assigned value.
    ValidateNumParameters(compiler, ++signature.arity);
    expression(compiler);
  }

  CallSignature(compiler, CODE_CALL_0, &signature);
}

static void call(bool canAssign)
{
  ignoreNewlines(compiler);
  Expect(TOKEN_NAME, "Expect method name after '.'.");
  namedCall(compiler, canAssign, CODE_CALL_0);
}

static void and_(bool canAssign)
{
  ignoreNewlines(compiler);

  // Skip the right argument if the left is false.
  int jump = emitJump(compiler, CODE_AND);
  parsePrecedence(compiler, PREC_LOGICAL_AND);
  PatchJump(compiler, jump);
}

static void or_(bool canAssign)
{
  ignoreNewlines(compiler);

  // Skip the right argument if the left is true.
  int jump = emitJump(compiler, CODE_OR);
  parsePrecedence(compiler, PREC_LOGICAL_OR);
  PatchJump(compiler, jump);
}

static void conditional(bool canAssign)
{
  // Ignore newline after '?'.
  ignoreNewlines(compiler);

  // Jump to the else branch if the condition is false.
  int ifJump = emitJump(compiler, CODE_JUMP_IF);

  // Compile the then branch.
  parsePrecedence(compiler, PREC_CONDITIONAL);

  Expect(TOKEN_COLON,
          "Expect ':' after then branch of conditional operator.");
  ignoreNewlines(compiler);

  // Jump over the else branch when the if branch is taken.
  int elseJump = emitJump(compiler, CODE_JUMP);

  // Compile the else branch.
  PatchJump(compiler, ifJump);

  parsePrecedence(compiler, PREC_ASSIGNMENT);

  // Patch the jump over the else.
  PatchJump(compiler, elseJump);
}

void infixOp(bool canAssign)
{
  GrammarRule* rule = getRule(compiler->parser->previous.type);

  // An infix operator cannot end an expression.
  ignoreNewlines(compiler);

  // Compile the right-hand side.
  parsePrecedence(compiler, (Precedence)(rule->precedence + 1));

  // Call the operator method on the left-hand side.
  Signature signature = { rule->name, (int)strlen(rule->name), SIG_METHOD, 1 };
  CallSignature(compiler, CODE_CALL_0, &signature);
}

void infixSignature(Signature* signature)
{
  // Add the RHS parameter.
  signature->type = SIG_METHOD;
  signature->arity = 1;

  // Parse the parameter name.
  Expect(TOKEN_LEFT_PAREN, "Expect '(' after operator name.");
  declareNamedVariable(compiler);
  Expect(TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");
}

void unarySignature(Signature* signature)
{
  // Do nothing. The name is already complete.
  signature->type = SIG_GETTER;
}

void mixedSignature(Signature* signature)
{
  signature->type = SIG_GETTER;

  // If there is a parameter, it's an infix operator, otherwise it's unary.
  if (Match(TOKEN_LEFT_PAREN))
  {
    // Add the RHS parameter.
    signature->type = SIG_METHOD;
    signature->arity = 1;

    // Parse the parameter name.
    declareNamedVariable(compiler);
    Expect(TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");
  }
}

static bool maybeSetter(Signature* signature)
{
  // See if it's a setter.
  if (!Match(TOKEN_EQ)) return false;

  // It's a setter.
  if (signature->type == SIG_SUBSCRIPT)
  {
    signature->type = SIG_SUBSCRIPT_SETTER;
  }
  else
  {
    signature->type = SIG_SETTER;
  }

  // Parse the value parameter.
  Expect(TOKEN_LEFT_PAREN, "Expect '(' after '='.");
  declareNamedVariable(compiler);
  Expect(TOKEN_RIGHT_PAREN, "Expect ')' after parameter name.");

  signature->arity++;

  return true;
}

void subscriptSignature(Signature* signature)
{
  signature->type = SIG_SUBSCRIPT;

  // The signature currently has "[" as its name since that was the token that
  // matched it. Clear that out.
  signature->length = 0;

  // Parse the parameters inside the subscript.
  finishParameterList(compiler, signature);
  Expect(TOKEN_RIGHT_BRACKET, "Expect ']' after parameters.");

  maybeSetter(compiler, signature);
}

void Parser::ParameterList(Signature* signature) {
  if (!Match(TOKEN_LEFT_PAREN)) return;

  signature->type = SIG_METHOD;

  // Allow an empty parameter list.
  if (Match(TOKEN_RIGHT_PAREN)) return;

  finishParameterList(compiler, signature);
  Expect(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
}

void Parser::NamedSignature(Signature* signature) {
  signature->type = SIG_GETTER;

  if (MaybeSetter(signature)) {
    return;
  }

  ParameterList(signature);
}

void Parser::ConstructorSignature(Signature* signature) {
  Expect(TOKEN_NAME, "Expect constructor name after 'construct'.");

  // capture the name.
  *signature = SignatureFromToken(SIG_INITIALIZER);

  if (Match(TOKEN_EQ)) {
    Error("A constructor cannot be a setter.");
    return;
  }

  if (!Match(TOKEN_LEFT_PAREN)) {
    Error("A constructor cannot be a getter.");
    return;
  }

  if (Match(TOKEN_RIGHT_PAREN)) {
    return;
  }

  FinishParemeterList(signature);
  Expect(TOKEN_RIGHT_PAREN, "Expect ')' after parameters.");
}




static GrammarRule* GetRule(TokenType type) {
  return &rules[type];
}

void Parser::ParsePrecedence(Precedence precedence) {
  NextToken();
  GrammarFn prefix = rules[lexer_.previous.type].prefix;

  if (prefix == nullptr) {
    Error("Expected expression.");
    return;
  }

  bool assignable = precedence <= PREC_CONDITIONAL;
  prefix(assignable);

  while (precedence <= rules[lexer_.current.type].precedence) {
    NextToken();
    GrammarFn infix = rules[lexer_.previous.type].infix;
    infix(assignable);
  }
}

void Parser::Expression() {
  ParsePrecedence(PREC_LOWEST);
}

int Parser::GetNumArguments(const uint8* bytecode, const Value* constants, int ip) {
  Code instruction = (Code)bytecode[ip];
  switch (instruction) {
    case CODE_NULL:
    case CODE_FALSE:
    case CODE_TRUE:
    case CODE_POP:
    case CODE_CLOSE_UPVALUE:
    case CODE_RETURN:
    case CODE_END:
    case CODE_LOAD_LOCAL_0:
    case CODE_LOAD_LOCAL_1:
    case CODE_LOAD_LOCAL_2:
    case CODE_LOAD_LOCAL_3:
    case CODE_LOAD_LOCAL_4:
    case CODE_LOAD_LOCAL_5:
    case CODE_LOAD_LOCAL_6:
    case CODE_LOAD_LOCAL_7:
    case CODE_LOAD_LOCAL_8:
    case CODE_CONSTRUCT:
    case CODE_FOREIGN_CONSTRUCT:
    case CODE_FOREIGN_CLASS:
    case CODE_END_MODULE:
      return 0;

    case CODE_LOAD_LOCAL:
    case CODE_STORE_LOCAL:
    case CODE_LOAD_UPVALUE:
    case CODE_STORE_UPVALUE:
    case CODE_LOAD_FIELD_THIS:
    case CODE_STORE_FIELD_THIS:
    case CODE_LOAD_FIELD:
    case CODE_STORE_FIELD:
    case CODE_CLASS:
      return 1;

    case CODE_CONSTANT:
    case CODE_LOAD_MODULE_VAR:
    case CODE_STORE_MODULE_VAR:
    case CODE_CALL_0:
    case CODE_CALL_1:
    case CODE_CALL_2:
    case CODE_CALL_3:
    case CODE_CALL_4:
    case CODE_CALL_5:
    case CODE_CALL_6:
    case CODE_CALL_7:
    case CODE_CALL_8:
    case CODE_CALL_9:
    case CODE_CALL_10:
    case CODE_CALL_11:
    case CODE_CALL_12:
    case CODE_CALL_13:
    case CODE_CALL_14:
    case CODE_CALL_15:
    case CODE_CALL_16:
    case CODE_JUMP:
    case CODE_LOOP:
    case CODE_JUMP_IF:
    case CODE_AND:
    case CODE_OR:
    case CODE_METHOD_INSTANCE:
    case CODE_METHOD_STATIC:
    case CODE_IMPORT_MODULE:
      return 2;

    case CODE_SUPER_0:
    case CODE_SUPER_1:
    case CODE_SUPER_2:
    case CODE_SUPER_3:
    case CODE_SUPER_4:
    case CODE_SUPER_5:
    case CODE_SUPER_6:
    case CODE_SUPER_7:
    case CODE_SUPER_8:
    case CODE_SUPER_9:
    case CODE_SUPER_10:
    case CODE_SUPER_11:
    case CODE_SUPER_12:
    case CODE_SUPER_13:
    case CODE_SUPER_14:
    case CODE_SUPER_15:
    case CODE_SUPER_16:
    case CODE_IMPORT_VARIABLE:
      return 4;

    case CODE_CLOSURE: {
      int constant = (bytecode[ip + 1] << 8) | bytecode[ip + 2];
      ObjFn* loadedFn = AS_FN(constants[constant]);

      // There are two bytes for the constant, then two for each upvalue.
      return 2 + (loadedFn->numUpvalues * 2);
    }
  }

  UNREACHABLE();
  return 0;
}

void Parser::StartLoop(Loop* loop) {
  loop->enclosing = loop_;
  loop->start = fn_->code.count - 1;
  loop->scope_depth = scope_depth_;
  loop_ = loop;
}

void Parser::TestExitLoop() {
  loop_->exit_jump = EmitJump(CODE_JUMP_IF);
}

void Parser::LoopBody() {
  loop_->body = fn_->code.count;
  Statement();
}

void Parser::EndLoop() {
  int loop_offset = fn_->code.count - loop_->start + 2;
  EmitShortArg(CODE_LOOP, loop_offset);

  PatchJump(loop_->exit_jump);

  int i = loop_->body;
  while (i < fn_->code.count) {
    if (fn_->code.data[i] == CODE_END) {
      fn_->code.data[i] = CODE_JUMP;
      PatchJump(i + 1);
      i += 3;
    } else {
      i += 1 + GetNumArguments(fn_->code.data, fn_->constants.data, i);
    }
  }

  loop_ = loop_->enclosing;
}

void Parser::ForStatement() {
  PushScope();

  Expect(TOKEN_LEFT_PARENT, "Expect '(' after 'for'.");
  Expect(TOKEN_NAME, "Expect for loop variable name.");

  const char* name = lexer_.current.start;
  int lnegth = lexer_.current.length;

  Expect(TOKEN_IN, "Expect 'in' after loop variable.");
  IgnoreNewLines();

  Expression();

  if (num_locals_ + 2 > MAX_LOCALS) {
    Error("Cannot declare more than %d variables in one scope. (Not enough space for for-loops internal variables)",
          MAX_LOCALS);
    return;
  }

  int seq_slot = AddLocal("seq ", 4);

  Null(false);
  int iter_slot = AddLocal("iter ", 5);

  Expect(TOKEN_RIGHT_PAREN, "Expect ')' after loop expression.");

  Loop loop;
  StartLoop(&loop);

  LoadLocal(seq_slot);
  LoadLocal(iter_slot);

  CallMethod(1, "iterate(_)", 10);
  EmitByteArg(CODE_STORE_LOCAL, iter_slot);
  TestExitLoop();

  LoadLocal(seq_slot);
  LoadLocal(iter_slot);
  CallMethod(1, "iteratorValue(_)", 16);

  PopScope();
  AddLocal(name, length);

  LoopBody();

  PopScope();

  EndLoop();

  PopScope();
}

void Parser::IfStatement() {
  Expect(TOKEN_LEFT_PAREN, "Expect '(' after 'if'.");
  expression(compiler);
  Expect(TOKEN_RIGHT_PAREN, "Expect ')' after if condition.");

  int if_jump = EmitJump(CODE_JUMF_IF);

  Statement();

  if (Match(TOKEN_ELSE)) {
    int else_jump = EmitJump(CODE_JUMP);
    PatchJump(if_jump);

    Statement();

    PatchJump(else_jump);
  } else {
    PatchJump(if_jump);
  }
}

void Parser::WhileStatement() {
  Loop loop;
  StartLoop(&loop);

  // Compile the condition.
  Expect(TOKEN_LEFT_PAREN, "Expect '(' after 'while'.");
  expression(compiler);
  Expect(TOKEN_RIGHT_PAREN, "Expect ')' after while condition.");

  testExitLoop(compiler);
  loopBody(compiler);
  endLoop(compiler);
}

void Parser::Statement() {
  if (Match(TOKEN_BREAK)) {
    if (loop_ == nullptr) {
      Error("Cannot use 'break' outside of a loop.");
      return;
    }

    DiscardLocals(loop_->scopeDepth + 1);

    EmitJump(CODE_END);
  } else if (Match(TOKEN_FOR)) {
    ForStatment();
  } else if (Match(TOKEN_IF)) {
    IfStatment();
  } else if (Match(TOKEN_RETURN)) {
    if (PeekToken() == TOKEN_LINE) {
      EmitOp(CODE_NULL);
    } else {
      Expression();
    }

    EmitOp(CODE_RETURN);
  } else if (Match(TOKEN_WHILE)) {
    WhileStatement();
  } else if (Match(TOKEN_LEFT_BRACE)) {
    PushScope();
    if (FinishBlock()) {
      EmitOp(CODE_POP);
    }
    PopScope();
  } else {
    Expression();
    EmitOp(CODE_POP);
  }
}

void Parser::Definition() {
  if (Match(TOKEN_CLASS)) {
    ClassDefinition(false);
  } else if (Match(TOKEN_FOREIGN)) {
    Expect(TOKEN_CLASS, "Expect 'class' after 'foreign'.");
    ClassDefinition(true);
  } else if (Match(TOKEN_IMPORT)) {
    Import();
  } else if (Match(TOKEN_VAR)) {
    VariableDefinition();
  } else {
    Statement();
  }
}

int ResolveLocal(const char* name, int length) {
  for (int i = num_locals_-1; i >= 0; --i) {
    if (locals_[i].length == length &&
        memcmp(name, locals_[i].name, length) == 0) {
      return i;
    }
  }

  return -1;
}

int AddUpvalue(bool local, int index) {
  for (int i = 0; i < fn_->num_upvalues; ++i) {
    CompilerUpvalue* upvalue = &upvalues_[i];
    if (upvalue->index == index && upvalue->local == local) {
      return i;
    }
  }
  
  upvalues[fn_->num_upvalues].local = local;
  upvalues[fn_->num_upvalues].index = index;
  return fn_->num_upvalues++;
}

int FindUpvalue(const char* name, int length) {
  if (parent_ == nullptr) {
    return -1;
  }
  
  if (name[0] != '_' && compiler->parent->enclosingClass != NULL) return -1;

  // See if it's a local variable in the immediately enclosing function.
  int local = ResolveLocal(parent_, name, length);
  if (local != -1) {
    compiler->parent->locals[local].isUpvalue = true;

    return addUpvalue(compiler, true, local);
  }
}

static Variable ResolveNonmodule(const char* name, int length) {
  Variable var;
  var.scope = SCOPE_LOCAL;
  var.index = ResolveLocal(name, length);
  if (var.index != -1) {
    return var;
  }
  
  var.scope = SCOPE_UPVALUE;
  var.index = FindUpvalue(name, length);
  return var;
}

Variable ResolveName(const char* name, int length) {
  Variable var = ResolveNonmodule(name, length);
  if (var.index != -1) {
    return var;
  }
  
  var.scope = SCOPE_MODULE;
  var.index = ffSymboleTableFind(&module_->variable_names_, name, length);
  return var;
}


void LoadLocal(int slot) {
  if (slot <= 8) {
    EmitOp((Code)(CODE_LOAD_LOCAL_0 + slot)); // no argument
    return;
  }
  
  EmitByteArg(CODE_LOAD_LOCAL, slot);
}



