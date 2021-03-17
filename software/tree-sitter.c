#include <ecl/ecl.h>
#include <locale.h>
#include <stdio.h>
#include <string.h>
#include "tree-sitter.h"
#include <time.h>
extern void init(cl_object);

char *package = "SOFTWARE-EVOLUTION-LIBRARY/SOFTWARE/TREE-SITTER";


/* Utility and debug functions. */

/* From ecl-devel mailing list */
cl_object utf8_encode_base_string(cl_object string) {
  cl_object output = ecl_alloc_adjustable_base_string(ecl_length(string));
  /* See https://common-lisp.net/project/ecl/static/manual/Streams.html#Sequence-Streams */
  cl_object stream = si_make_sequence_output_stream(3, output,
                                                    ecl_make_keyword("EXTERNAL-FORMAT"),
                                                    ecl_make_keyword("UTF-8"));
  cl_write_sequence(2, string, stream);
  ecl_write_char(0, stream); /* write null terminator */
  return output;
}

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
  return ecl_base_string_pointer_safe(utf8_encode_base_string(ecl_null_terminated_base_string(cl_object)));
}

char* to_string(cl_object cl_object){
  return get_string(cl_format(3, c_string_to_object("nil"), c_string_to_object("\"~&~S\""), cl_object));
}

short to_short(cl_object cl_object){
  return ecl_to_short(cl_object);
}

void show(cl_object cl_object){
  cl_format(3, c_string_to_object("t"), c_string_to_object("\"~&; ~S~%\""), cl_object);
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
  return cl_funcall(3, ecl_make_symbol("CONVERT", package),
                    language_symbol(language),
                    ecl_cstring_to_base_string_or_nil(source));
                    /* ecl_make_constant_base_string(source, strlen(source))); */
                    /* ecl_make_simple_base_string(source, strlen(source))); */
}

cl_object get_type(cl_object cl_object){
  return cl_type_of(cl_object);
}

cl_object get_class(cl_object cl_object){
  return cl_funcall(2, ecl_make_symbol("CLASS-NAME", package), (cl_class_of(cl_object)));
}

char* symbol_name(cl_object cl_object){
  return to_string(cl_funcall(2, ecl_make_symbol("SYMBOL-NAME", package), cl_object));
}

/* // Alternate implementation taking a single position offset into the text string.
 * cl_object ast_at_point(cl_object ast, int position){
 *   return cl_car(cl_last(1, cl_funcall(3, c_string_to_object("asts-containing-source-location"),
 *                                       ast,
 *                                       position)));
 * }
*/

cl_object ast_at_point(cl_object ast, int line, int column){
  return cl_car(cl_last(1, cl_funcall(3,
                                      ecl_make_symbol("ASTS-CONTAINING-SOURCE-LOCATION", package),
                                      ast,
                                      cl_funcall(6,
                                                 ecl_make_symbol("MAKE-INSTANCE", package),
                                                 ecl_make_symbol("SOURCE-LOCATION", package),
                                                 ecl_make_keyword("LINE"), line,
                                                 ecl_make_keyword("COLUMN"), column))));
}

char* source_text(cl_object ast){
  return get_string(cl_funcall(2, ecl_make_symbol("SOURCE-TEXT", package), ast));
}

cl_object children(cl_object ast){
  return(cl_funcall(2, ecl_make_symbol("CHILDREN", package), ast));
}

cl_object child_slots(cl_object ast){
  return cl_funcall(2, ecl_make_symbol("CHILD-SLOTS", package), ast);
}

cl_object slot(cl_object ast, const char* slot_name){
  return ecl_slot_value(ast, slot_name);
}

cl_object parent(cl_object root, cl_object ast){
  return cl_funcall(4, ecl_make_symbol("GET-PARENT-AST", package), root, ast);
}

#define type_check(NAME) if(! null(cl_funcall(3, ecl_make_symbol("SUBTYPEP", package), \
                                              cl_funcall(2, ecl_make_symbol("TYPE-OF", package), \
                                                         ast),          \
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
  return cl_funcall(3, ecl_make_symbol("REMOVE-IF-NOT", package),
                    c_string_to_object("{typep _ 'function-ast}"),
                    ast);
}

char* function_name(cl_object ast){
  return get_string(cl_funcall(2, ecl_make_symbol("FUNCTION-NAME", package), ast));
}

cl_object function_parameters(cl_object ast){
  return cl_funcall(2, ecl_make_symbol("FUNCTION-PARAMETERS", package), ast);
}

cl_object function_body(cl_object ast){
  return cl_funcall(2, ecl_make_symbol("FUNCTION-BODY", package), ast);
}

cl_object call_asts(cl_object ast){
  return cl_funcall(3, ecl_make_symbol("REMOVE-IF-NOT", package),
                    c_string_to_object("{typep _ 'call-ast}"),
                    ast);
}

cl_object call_arguments(cl_object ast){
  return cl_funcall(2, ecl_make_symbol("CALL-ARGUMENTS", package), ast);
}

cl_object call_module(cl_object ast){
  return cl_funcall(2, ecl_make_symbol("CALL-MODULE", package), ast);
}

cl_object call_function(cl_object ast){
  return cl_funcall(2, ecl_make_symbol("CALL-FUNCTION", package), ast);
}
