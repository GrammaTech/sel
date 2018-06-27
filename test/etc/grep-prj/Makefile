CC = clang
CFLAGS =

SOURCES = dfa.c grep.c kwset.c obstack.c regex.c search.c
OBJECTS = $(SOURCES:.c=.o)

all: grep

%.o: %.c
	$(CC) $(CFLAGS) -c -D GREP -D STDC_HEADERS -D HAVE_STRING_H -D HAVE_SYS_PARAM_H -D HAVE_UNISTD_H -D HAVE_ALLOCA_H -D HAVE_GETPAGESIZE -D HAVE_MEMCHR -D HAVE_STRERROR -D HAVE_VALLOC -D HAVE_WORKING_MMAP -D LINUX -D _GNU_SOURCE -std=c99 -o $@ $<

grep: $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -rf $(OBJECTS)
	rm -rf grep
