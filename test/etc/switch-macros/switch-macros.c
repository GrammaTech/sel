int main (int argc, char **argv)
{
  int c;

  switch (c)
  {
#define CASE_OLD_ARG(old_char,new_string)       \
    case old_char:                              \
      new_string;	\
      break

  case 'F': /* obsolescent and undocumented alias */
    CASE_OLD_ARG ('f', "fF");
#undef CASE_OLD_ARG
  }

  return 1;
}
