CC=gfortran
CFLAGS=-Wall -std=f2008ts -fimplicit-none -ftree-vectorizer-verbose=2 -Wno-maybe-uninitialized -static-libgfortran -flto

all:
	$(CC) $(CFLAGS) -o bin/app src/app.f90

clean:
	rm -f bin/*.*

run:
	cd ./bin; ./app;
	cat ./bin/output.txt
