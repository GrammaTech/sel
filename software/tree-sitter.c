#include <ecl/ecl.h>
#include "tree-sitter.h"
extern void init(cl_object);

void start(){
  int argc = 0;
  char** argv = (char*[]){""};

  cl_boot(argc, argv);
  ecl_init_module(NULL, init);
}

void stop(){
  cl_shutdown();
}

cl_object convert(char *source){
  cl_object text = ecl_cstring_to_base_string_or_nil(source);
  return cl_funcall(3, c_string_to_object("convert"), c_string_to_object(":python"), text);
}

cl_object eval(char *source){
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
