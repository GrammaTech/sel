#include <ecl/ecl.h>
#include <locale.h>
#include <stdio.h>
#include <string.h>
#include "tree-sitter.h"
#include <time.h>
extern void init(cl_object);

char *package = "SOFTWARE-EVOLUTION-LIBRARY/SOFTWARE/TREE-SITTER";


/* Utility and debug functions. */

/*
 * 5:59:00 jackdaniel eschulte: ecl_base_string_pointer_safe takes a
 *                    common lisp object and if it is a base string, it
 *                    returns it
 *
 * 5:59:36 jackdaniel moreover, if the string is not guaranteed to be a
 *                    base string, ecl_null_terminated_base_string will
 *                    try to coerce it
 */
char* get_string(cl_object cl_object){
  return ecl_base_string_pointer_safe(ecl_null_terminated_base_string(cl_object));
}

char* to_string(cl_object cl_object){
  return get_string(cl_funcall(4, c_string_to_object("format"),
                               c_string_to_object("nil"),
                               c_string_to_object("\"~&~S\""),
                               cl_object));
}

short to_short(cl_object cl_object){
  return ecl_to_short(cl_object);
}

void show(cl_object cl_object){
  cl_funcall(4, c_string_to_object("format"),
             c_string_to_object("t"),
             c_string_to_object("\"~&; ~S~%\""),
             cl_object);
}

cl_object eval(char* source){
  return cl_eval(c_string_to_object(source));
}

cl_object language_symbol(language language){
  switch(language){
  case JAVASCRIPT: return ecl_make_symbol("JAVASCRIPT-AST", package);
  case PYTHON: return ecl_make_symbol("PYTHON-AST", package);
  case C: return ecl_make_symbol("C-AST", package);
  case CPP: return ecl_make_symbol("CPP-AST", package);
  case UNKNOWN_LANGUAGE: return ecl_make_symbol("UNKNOWN_LANGUAGE", package);
  }
  return ECL_NIL;
}

cl_object car(cl_object list){
  return ecl_car(list);
}

cl_object cdr(cl_object list){
  return ecl_cdr(list);
}

bool null(cl_object object){
  return ecl_eql(object, ECL_NIL);
}

bool eql(cl_object left, cl_object right){
  return ecl_eql(left, right);
}


/* API functions */
#ifdef DEBUG
void put_time(){
  time_t timer;
  char buffer[26];
  struct tm* tm_info;

  timer = time(NULL);
  tm_info = localtime(&timer);

  strftime(buffer, 26, "%Y-%m-%d %H:%M:%S", tm_info);
  puts(buffer);
}
#endif

void start(){
  int argc = 0;
  char** argv = (char*[]){""};
  setlocale(LC_ALL, "");

#ifdef DEBUG
  put_time();
  cl_boot(argc, argv);
  put_time();
  ecl_init_module(NULL, init);
  put_time();
#else
  cl_boot(argc, argv);
  ecl_init_module(NULL, init);
#endif
}

void stop(){
  cl_shutdown();
}

cl_object convert(language language, char* source){
  return cl_funcall(3, c_string_to_object("convert"),
                    language_symbol(language),
                    ecl_cstring_to_base_string_or_nil(source));
                    /* ecl_make_constant_base_string(source, strlen(source))); */
                    /* ecl_make_simple_base_string(source, strlen(source))); */
}

cl_object get_type(cl_object cl_object){
  return cl_type_of(cl_object);
}

cl_object get_class(cl_object cl_object){
  return cl_funcall(2, c_string_to_object("class-name"), (cl_class_of(cl_object)));
}

char* symbol_name(cl_object cl_object){
  return to_string(cl_funcall(2, c_string_to_object("symbol-name"), cl_object));
}

/* // Alternate implementation taking a single position offset into the text string.
 * cl_object ast_at_point(cl_object ast, int position){
 *   return cl_car(cl_last(1, cl_funcall(3, c_string_to_object("asts-containing-source-location"),
 *                                       ast,
 *                                       position)));
 * }
*/

cl_object ast_at_point(cl_object ast, int line, int column){
  return cl_car(cl_last(1, cl_funcall(3, c_string_to_object("asts-containing-source-location"),
                                      ast,
                                      cl_funcall(6, c_string_to_object("make-instance"),
                                                 ecl_make_symbol("SOURCE-LOCATION", package),
                                                 ecl_make_keyword("LINE"), line,
                                                 ecl_make_keyword("COLUMN"), column))));
}

char* source_text(cl_object ast){
  return get_string(cl_funcall(2, c_string_to_object("source-text"), ast));
}

cl_object children(cl_object ast){
  return(cl_funcall(2, c_string_to_object("children"), ast));
}

cl_object child_slots(cl_object ast){
  return cl_funcall(2, c_string_to_object("child-slots"), ast);
}

cl_object slot(cl_object ast, const char* slot_name){
  return ecl_slot_value(ast, slot_name);
}

cl_object parent(cl_object root, cl_object ast){
  return cl_funcall(4, c_string_to_object("get-parent-ast"), root, ast);
}

#define type_check(NAME) if(! null(cl_funcall(3, c_string_to_object("subtypep"), \
                       cl_funcall(2, c_string_to_object("type-of"), ast), \
                       ecl_make_symbol( #NAME "-AST", package)))) \
    return NAME

language ast_language(cl_object ast){
  type_check(PYTHON);
  else type_check(JAVASCRIPT);
  else type_check(C);
  else type_check(CPP);
  else return UNKNOWN_LANGUAGE;
}

type ast_type(cl_object ast){
  type_check(PARSE_ERROR);
  else type_check(CHAR);
  else type_check(GOTO);
  else type_check(COMPOUND);
  else type_check(CLASS);
  else type_check(IF);
  else type_check(WHILE);
  else type_check(CONTROL_FLOW);
  else type_check(FUNCTION);
  else type_check(BOOLEAN_TRUE);
  else type_check(BOOLEAN_FALSE);
  else type_check(IDENTIFIER);
  else type_check(LAMBDA);
  else type_check(INTEGER);
  else type_check(FLOAT);
  else type_check(NUMBER);
  else type_check(STRING);
  else type_check(LOOP);
  else type_check(CALL);
  else type_check(UNARY);
  else type_check(BINARY);
  else type_check(RETURN);
  else type_check(VARIABLE_DECLARATION);
  else type_check(STATEMENT);
  else type_check(EXPRESSION);
  else return UNKNOWN_TYPE;
}

bool subtypep(cl_object ast, char* type_name){
  return ! ecl_eql(ECL_NIL, cl_subtypep(2, ast, ecl_make_symbol(type_name, package)));
}

/* General methods */
cl_object function_asts(cl_object ast){
  return cl_funcall(3, c_string_to_object("remove-if-not"),
                    c_string_to_object("{typep _ 'function-ast}"),
                    ast);
}

char* function_name(cl_object ast){
  return get_string(cl_funcall(2, c_string_to_object("function-name"), ast));
}

cl_object function_parameters(cl_object ast){
  return cl_funcall(2, c_string_to_object("function-parameters"), ast);
}

cl_object function_body(cl_object ast){
  return cl_funcall(2, c_string_to_object("function-body"), ast);
}

cl_object call_asts(cl_object ast){
  return cl_funcall(3, c_string_to_object("remove-if-not"),
                    c_string_to_object("{typep _ 'call-ast}"),
                    ast);
}

cl_object call_arguments(cl_object ast){
  return cl_funcall(2, c_string_to_object("call-arguments"), ast);
}

cl_object call_module(cl_object ast){
  return cl_funcall(2, c_string_to_object("call-module"), ast);
}

cl_object call_function(cl_object ast){
  return cl_funcall(2, c_string_to_object("call-function"), ast);
}
