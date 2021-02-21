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
extern cl_object convert(language language, char* source);
extern void show(cl_object cl_object);
extern short to_short(cl_object cl_object);
extern wchar_t* to_string(cl_object cl_object);
extern wchar_t* type(cl_object cl_object);
extern cl_object ast_at_point(cl_object ast, int line, int column);
extern wchar_t* source_text(cl_object ast);
extern cl_object children(cl_object ast);
extern cl_object car(cl_object list);
extern cl_object cdr(cl_object list);
extern bool null(cl_object cl_object);
extern bool eql(cl_object left, cl_object right);
extern language ast_language(cl_object ast);
extern cl_object child_slots(cl_object ast);
extern cl_object slot(cl_object ast, const char* slot_name);
