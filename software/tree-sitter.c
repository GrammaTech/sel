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
  return NULL;
}
