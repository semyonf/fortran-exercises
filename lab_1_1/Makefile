CC=gfortran
SRC_FILES = $(wildcard src/*.f90)
CFLAGS=-Wall -std=f2008ts -fimplicit-none -ftree-vectorizer-verbose=2 -Wno-maybe-uninitialized -static-libgfortran -flto $(SRC_FILES)

all:
	$(CC) $(CFLAGS) -o bin/exec

clean:
	rm -f bin/*

run:
	cd ./bin; ./exec;
	cat ./bin/output.txt