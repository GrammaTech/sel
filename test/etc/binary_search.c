#include <stdio.h>
#include <stdlib.h>

#include "read_words.h"

int binary_search(const char *needle, char **haystack, int start, int end) {
    int mid;
    int rc;
    if (start == end) {
        return -1;
    }

    mid = (start + end) / 2;
    rc = strcmp(needle, haystack[mid]);
    if (rc < 0) {
        return binary_search(needle, haystack, start, mid);
    } else if (rc > 0) {
        return binary_search(needle, haystack, mid + 1, end);
    }

    return mid;
}

int main(int argc, char** argv) {
  if(argc != 6) {
    printf("USAGE: binary_search [STRING] [NUM_WORDS] [START] ");
    printf("[END] [FILE]\n");
    return 1;
  }
  else {
    char *needle = argv[1];
    unsigned int haystack_size = atoi(argv[2]);
    unsigned int start = atoi(argv[3]);
    unsigned int end = atoi(argv[4]);
    char *filename = argv[5];
    char **haystack = malloc(haystack_size * sizeof(*haystack));
    if(haystack) {
      FILE *fp = fopen(filename, "r");
      unsigned int index = -2;
      size_t i;
      for(i = 0; i<haystack_size; ++i) {
        haystack[i] = malloc(256 * sizeof(*haystack[i]));
      }
      read_words(fp, haystack);
      index = binary_search(needle, haystack, start, end);
      fprintf(stdout, "%d", index);
      fclose(fp);

      for(i=0; i<haystack_size; ++i) {
        free(haystack[i]);
      }
      free(haystack);
    }
  }
  return EXIT_SUCCESS;
}
