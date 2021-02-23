#include <ecl/ecl.h>

extern size_t get_last_string_length(void);
extern void start(void);
extern void stop(void);
extern cl_object eval(char* source);
typedef enum {
  UNKNOWN_LANGUAGE,
  JAVASCRIPT,
  PYTHON,
  C,
  CPP
} language;
typedef enum {
  UNKNOWN_TYPE,
  PARSE_ERROR,
  CHAR,
  NUMBER,
  GOTO,
  COMPOUND,
  CLASS,
  CONTROL_FLOW,
  IF,
  WHILE,
  EXPRESSION,
  FUNCTION,
  BOOLEAN_TRUE,
  BOOLEAN_FALSE,
  IDENTIFIER,
  LAMBDA,
  INTEGER,
  FLOAT,
  STRING,
  LOOP,
  STATEMENT,
  CALL,
  UNARY,
  BINARY,
  RETURN,
  VARIABLE_DECLARATION,
} type;
extern cl_object convert(language language, char* source);
extern void show(cl_object cl_object);
extern short to_short(cl_object cl_object);
extern wchar_t* to_string(cl_object cl_object);
extern cl_object get_type(cl_object cl_object);
extern cl_object get_class(cl_object cl_object);
extern wchar_t* symbol_name(cl_object cl_object);
extern cl_object ast_at_point(cl_object ast, int line, int column);
extern wchar_t* source_text(cl_object ast);
extern cl_object children(cl_object ast);
extern cl_object car(cl_object list);
extern cl_object cdr(cl_object list);
extern bool null(cl_object cl_object);
extern bool eql(cl_object left, cl_object right);
extern language ast_language(cl_object ast);
extern type ast_type(cl_object ast);
extern type ast_type(cl_object ast);
extern cl_object child_slots(cl_object ast);
extern cl_object slot(cl_object ast, const char* slot_name);
extern bool subtypep(cl_object ast, char* type_name);
extern wchar_t* function_name(cl_object ast);
extern cl_object function_parameters(cl_object ast);
extern cl_object function_body(cl_object ast);
