FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto
FOPT=-O3 -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) $(FOPT) -c src/lab_1_2.f90 -I obj/ -o obj/lab_1_2.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/lab_1_2 ./obj/environment.o obj/lab_1_2.o

clean:
	rm -f obj/*
	rm -f bin/*

run:
	cd ./bin; ./lab_1_2
	cat ./bin/output.txt
