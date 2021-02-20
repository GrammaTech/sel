#include <ecl/ecl.h>

extern void start();
extern void stop();
extern cl_object eval(char *source);
typedef enum {
  JAVASCRIPT,
  PYTHON,
  C,
  CPP
} language;
extern cl_object convert(language language, char *source);
extern wchar_t* type(cl_object cl_object);
