#include <ecl/ecl.h>
#include <locale.h>
#include <stdio.h>
#include <string.h>
#include "tree-sitter.h"

#define check(NAME, LEFT, RIGHT) if(LEFT == RIGHT){ printf("PASS\n"); } else { \
    printf("FAIL: %d != %d for " #NAME "\n", LEFT, RIGHT);              \
    success = 1;                                                        \
  }

int main(int argc, char** argv){
  int success = 0;
  start();
  char* source = "x + 88";
  cl_object ast = convert(PYTHON, source);
  printf("Source length %zu\n", get_last_string_length());
  printf("AST pointer %p\n", (void*)ast);
  show(ast);
  check(LANGUAGE, ast_language(ast), PYTHON);
  check(TYPE, ast_type(ast), UNKNOWN_TYPE);
  check(SOURCE-TEXT-LENGTH, strlen(source), strlen(source_text(ast)));
  stop();
  return success;
}
