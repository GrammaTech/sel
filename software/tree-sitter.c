#include <ecl/ecl.h>
#include <locale.h>
#include <stdio.h>
#include <string.h>
#include "tree-sitter.h"
extern void init(cl_object);

char *package = "SOFTWARE-EVOLUTION-LIBRARY/SOFTWARE/TREE-SITTER";

size_t last_string_length;

wchar_t* get_string(cl_object cl_object){
  last_string_length = cl_object->string.dim;
  #ifdef DEBUG
  fprintf(stderr, "; Returning string: '%ls'\n", cl_object->string.self);
  #endif
  return cl_object->string.self;
}

size_t get_last_string_length(){
  return last_string_length;
}

void start(){
  int argc = 0;
  char** argv = (char*[]){""};

  setlocale(LC_ALL, "");
  cl_boot(argc, argv);
  ecl_init_module(NULL, init);
}

void stop(){
  cl_shutdown();
}

void show(cl_object cl_object){
  cl_funcall(4, c_string_to_object("format"),
             c_string_to_object("t"),
             c_string_to_object("\"~&; ~S~%\""),
             cl_object);
}

wchar_t* to_string(cl_object cl_object){
  return get_string(cl_funcall(4, c_string_to_object("format"),
                               c_string_to_object("nil"),
                               c_string_to_object("\"~&~S\""),
                               cl_object));
}

short to_short(cl_object cl_object){
  return ecl_to_short(cl_object);
}

cl_object eval(char* source){
  cl_env_ptr env = ecl_process_env();
  ECL_CATCH_ALL_BEGIN(env) {
    /*
     * Code that is protected. Uncaught lisp conditions, THROW,
     * signals such as SIGSEGV and SIGBUS may cause jump to
     * this region.
     */
    return cl_eval(c_string_to_object(source));
  } ECL_CATCH_ALL_IF_CAUGHT {
    /*
     * If the exception, lisp condition or other control transfer
     * is caught, this code is executed.
     */
    return ecl_make_keyword("ERROR");
  } ECL_CATCH_ALL_END;
}

cl_object language_symbol(language language){
  switch(language){
  case JAVASCRIPT: return ecl_make_symbol("JAVASCRIPT-AST", package);
  case PYTHON: return ecl_make_symbol("PYTHON-AST", package);
  case C: return ecl_make_symbol("C-AST", package);
  case CPP: return ecl_make_symbol("CPP-AST", package);
  case UNKNOWN_LANGUAGE: return ecl_make_symbol("ERROR", package);
  }
  return ecl_make_symbol("ERROR", package);  
}

cl_object convert(language language, char* source){
  cl_env_ptr env = ecl_process_env();
  ECL_CATCH_ALL_BEGIN(env) {
    /*
     * Code that is protected. Uncaught lisp conditions, THROW,
     * signals such as SIGSEGV and SIGBUS may cause jump to
     * this region.
     */
  return cl_funcall(3, c_string_to_object("convert"),
                    language_symbol(language),
                    /* ecl_cstring_to_base_string_or_nil(source)); */
                    ecl_make_constant_base_string(source, strlen(source)));
  } ECL_CATCH_ALL_IF_CAUGHT {
    /*
     * If the exception, lisp condition or other control transfer
     * is caught, this code is executed.
     */
    return ecl_make_keyword("ERROR");
  } ECL_CATCH_ALL_END;
}

wchar_t* get_type(cl_object cl_object){
  return get_string(cl_symbol_name(cl_type_of(cl_object)));
}

wchar_t* get_class(cl_object cl_object){
  return get_string(cl_symbol_name(cl_funcall(2, c_string_to_object("class-name"),
                                              (cl_class_of(cl_object)))));
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

wchar_t* source_text(cl_object ast){
  return get_string(cl_funcall(2, c_string_to_object("source-text"), ast));
}

cl_object children(cl_object ast){
  return(cl_funcall(2, c_string_to_object("children"), ast));
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

language ast_language(cl_object ast){
  if(! null(cl_funcall(3, c_string_to_object("subtypep"),
                       cl_funcall(2, c_string_to_object("type-of"), ast),
                       ecl_make_symbol("PYTHON-AST", package))))
    return PYTHON;
  else if(! null(cl_funcall(3, c_string_to_object("subtypep"),
                       cl_funcall(2, c_string_to_object("type-of"), ast),
                       ecl_make_symbol("JAVASCRIPT-AST", package))))
    return JAVASCRIPT;
  else if(! null(cl_funcall(3, c_string_to_object("subtypep"),
                       cl_funcall(2, c_string_to_object("type-of"), ast),
                       ecl_make_symbol("C-AST", package))))
    return C;
  else if(! null(cl_funcall(3, c_string_to_object("subtypep"),
                       cl_funcall(2, c_string_to_object("type-of"), ast),
                       ecl_make_symbol("CPP-AST", package))))
    return CPP;
  else
    return UNKNOWN_LANGUAGE;
}

cl_object child_slots(cl_object ast){
  return cl_funcall(2, c_string_to_object("child-slots"), ast);
}

cl_object slot(cl_object ast, const char* slot_name){
  return ecl_slot_value(ast, slot_name);
}

/* General methods */
wchar_t* function_name(cl_object ast){
  return get_string(cl_funcall(2, c_string_to_object("function-name"), ast));
}
