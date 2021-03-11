#include <ecl/ecl.h>
#include <locale.h>
#include <stdio.h>
#include <string.h>
#include <wchar.h>
#include "tree-sitter.h"

#define check(NAME, TYPE, LEFT, RIGHT) if(LEFT == RIGHT){ printf("PASS\n"); } else { \
    printf("FAIL: "#TYPE" != "#TYPE" for "#NAME"\n", LEFT, RIGHT);            \
    success = 1;                                                        \
  }

/*
int main(int argc, char** argv){
  int success = 0;
  start();
  char* source = "x + 88";
  cl_object ast = convert(PYTHON, source);
  printf("AST pointer %p\n", (void*)ast);
  show(ast);

  check(LANGUAGE, %d, ast_language(ast), PYTHON);
  check(TYPE, %d, ast_type(ast), UNKNOWN_TYPE);
  check(SOURCE-TEXT-LENGTH, %lu, strlen(source), wcslen(source_text(ast)));

  stop();
  return success;
}
*/

int main(int argc, char** argv){
  start();
  /* eval("(trace convert)"); */
  /* eval("(defmethod check-interleaved-text ((ast python-ast)) nil)"); */
  /* eval("(trace parse-string)"); */

  cl_object convert = c_string_to_object("convert");
  show(convert);
  cl_object symbol = ecl_make_symbol("PYTHON-AST",
                                     "SOFTWARE-EVOLUTION-LIBRARY/SOFTWARE/TREE-SITTER");
  show(symbol);
  char* source = "x + 88";
  cl_object str = ecl_cstring_to_base_string_or_nil(source);
  show(str);
  cl_object ast = cl_funcall(3, convert, symbol, str);
  show(ast);
  printf("SOURCE-TEXT:\"%s\"\n", source_text(ast));

  return 0;
}