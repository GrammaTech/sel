#include <stdio.h>

char** read_words(FILE* fp, char** words) {
  unsigned int char_index = 0;
  unsigned int words_index = 0;
  char ch = fgetc(fp);
  while(ch != EOF) {
    if(ch == '\n' || char_index >= 255) {
      words[words_index][char_index] = '\0';
      words_index += 1;
      char_index = 0;
    }
    else {
      words[words_index][char_index] = ch;
      char_index += 1;
    }
    ch = fgetc(fp);
  }
  return words;
}
