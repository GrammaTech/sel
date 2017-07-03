#include <stdio.h>
#include <stdlib.h>

int main(int argc, char* argv[]) {
	if (argc >= 2) {
		int x;
		x = atoi(argv[1]);
		if (x >= 5) {
			printf("x is larger than 5\n");
		} else {
			printf("x is 5 or smaller\n");
		}
	}
	return EXIT_SUCCESS;
}
