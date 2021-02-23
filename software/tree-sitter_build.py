import os
import platform
from cffi import FFI
ffibuilder = FFI()
this_dir = os.path.dirname(os.path.abspath(__file__))

# cdef() expects a single string declaring the C types, functions and
# globals needed to use the shared object. It must be in valid C syntax.
ffibuilder.cdef("""
extern void start();
extern void stop();
extern void show(void* cl_object);
extern short to_short(void* cl_object);
extern wchar_t* to_string(void* cl_object);
extern void* eval(char* source);
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
extern void* convert(language language, char* source);
extern void* get_type(void* cl_object);
extern void* get_class(void* cl_object);
extern wchar_t* symbol_name(void* cl_object);
extern void* ast_at_point(void* ast, int line, int column);
extern wchar_t* source_text(void* ast);
extern void* children(void* ast);
extern void* car(void* list);
extern void* cdr(void* list);
extern bool null(void* cl_object);
extern bool eql(void* left, void* right);
extern language ast_language(void* ast);
extern type ast_type(void* ast);
extern void* child_slots(void* ast);
extern void* slot(void* ast, const char* slot_name);
extern bool subtypep(void* ast, char* type_name);
extern size_t get_last_string_length();
extern wchar_t* function_name(void* ast);
extern void* function_parameters(void* ast);
extern void* function_body(void* ast);
""")

if platform.system() == "Darwin":
    object_extension = "dylib"
elif platform.system() == "Windows":
    object_extension = "dll"
elif platform.system() == "Linux":
    object_extension = "so"

# set_source() gives the name of the python extension module to
# produce, and some C source code as a string.  This C code needs
# to make the declarated functions, types and globals available,
# so it is often just the "#include".
ffibuilder.set_source("_tree_sitter_cffi",
"""
     #include "tree-sitter.h"   // the C header of the library
""",
                      sources=[this_dir+'/tree-sitter.c'],
                      include_dirs=[this_dir],
                      libraries=['ecl'],
                      extra_objects=[this_dir+'/tree-sitter--all-systems.'+object_extension])

if __name__ == "__main__":
    ffibuilder.compile(verbose=True)
