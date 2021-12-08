#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/time.h>
#include <sys/resource.h>

int main(int argc, char *argv[]) {

  // resource/time limits
  struct rlimit limit;

  // parse command line arguments
  int i = 1;
  int seconds = 4;

  if (argc > 1 && (strcmp(argv[i], "-s") == 0 ||
                   strcmp(argv[i], "--seconds") == 0)) {
    seconds = atoi(argv[++i]);
    assert(seconds > 0);
    i++;
  }

  // set limits
  limit.rlim_cur = seconds; limit.rlim_max = seconds;
  setrlimit(RLIMIT_CPU, &limit);       // cpu seconds
  limit.rlim_cur = 16384; limit.rlim_max = 16384;
  setrlimit(RLIMIT_NPROC, &limit);     // number of spawned processes
  limit.rlim_cur = 16384; limit.rlim_max = 16384;
  setrlimit(RLIMIT_NOFILE, &limit);    // number of open files
  limit.rlim_cur = 1073741824; limit.rlim_max = 1073741824;
  setrlimit(RLIMIT_FSIZE, &limit);     // max file size (bytes)
  setrlimit(RLIMIT_MEMLOCK, &limit);   // max memory locked into RAM (bytes)
  setrlimit(RLIMIT_STACK, &limit);     // max stack size (bytes)
  alarm(seconds);                      // wall clock seconds

  // run
  return execvp(argv[i], &argv[i]);
}
