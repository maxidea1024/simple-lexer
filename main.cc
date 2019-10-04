#include "lexer.h"

int main(int argc, char* argv[]) {
  Lexer l;

  //l.Init("\"${a}..${b}..${c}..!\"");
  //l.Init("\"|----${a}----${b}----${c}----|\"");
  l.Init("true false if while var val yield resume");

  while (l.current.type != TOKEN_EOF) {
    //printf("%04d> %-12s : %s\n", l.current.line, l.current.TypeName().c_str(),
    //       l.current.value.ToString().c_str());
    printf("%04d>> %s\n", l.current.line, l.current.ToString().c_str());
    if (l.current.type == TOKEN_INTERPOLATION) {
      printf("....%04d>> %s\n", l.previous.line, l.previous.ToString().c_str());
    }
    l.NextToken();
  }

  return 0;
}
