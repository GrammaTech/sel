#include <stdio.h>
#include <stdlib.h>
// INSTRUMENT SHOULD ADD THE FOLLOWING LINE
//#include "abst_cond.h"

int main(int argc, char* argv[]) {
  // INSTRUMENT SHOULD ADD ONE OF THE FOLLOWING SETS OF LINES
	/*
  set_abst_cond_default_val(1);
  /* */
  /*
  set_abst_cond_return_vals(atoi(argv[1]), argv[2]);
  argv[2] = argv[0];
  argv += 2;
  argc -= 2;
  /* */
  // END INSTRUMENTED LINES

	if (argc >= 2) {
		int x;
		x = atoi(argv[1]);
  	// INSTRUMENT SHOULD ADD abst_cond() IN FOLLOWING LINE
		if (x > 5) {
			printf("x is 5 or larger\n");
		} else {
			printf("x is smaller than 5\n");
		}
	}
	return EXIT_SUCCESS;
}
