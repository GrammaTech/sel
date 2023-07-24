CFLAGS = -std=c++20
CC = clang++

all: main
	$(CC) $(CFLAGS) b-interface_part.pcm -c -o b-interface_part.o
	$(CC) $(CFLAGS) b-impl_part.pcm -c -o b-impl_part.o
	$(CC) $(CFLAGS) b.pcm -c -o b.o
	$(CC) $(CFLAGS) a-interface_part.pcm -c -o a-interface_part.o
	$(CC) $(CFLAGS) a-impl_part.pcm -c -o a-impl_part.o
	$(CC) $(CFLAGS) a.pcm -c -o a.o
	$(CC) *.o -o main

main: a
	$(CC) $(CFLAGS) main.cc -fprebuilt-module-path=. -c -o main.o

a: a-interface_part a-impl_part
	$(CC) $(CFLAGS) a.cppm --precompile -fprebuilt-module-path=. -o a.pcm

a-impl_part: b a-interface_part
	$(CC) $(CFLAGS) a-impl_part.cppm --precompile -fprebuilt-module-path=. -o a-impl_part.pcm

a-interface_part:
	$(CC) $(CFLAGS) a-interface_part.cppm --precompile -o a-interface_part.pcm


b: b-interface_part b-impl_part
	$(CC) $(CFLAGS) b.cppm --precompile -fprebuilt-module-path=. -o b.pcm

b-impl_part: b-interface_part
	$(CC) $(CFLAGS) b-impl_part.cppm --precompile -fprebuilt-module-path=. -o b-impl_part.pcm

b-interface_part:
	$(CC) $(CFLAGS) b-interface_part.cppm --precompile -o b-interface_part.pcm

clean:
	rm -f *.pcm *.o main

allclean: clean
	rm -f *~
