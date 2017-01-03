FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto
FOPT=-O3 -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/source_io.f90 -J obj/ -o obj/source_io.o
	$(FC) $(FFLAGS) $(FOPT) -c src/source_process.f90 -J obj/ -o obj/source_process.o
	$(FC) $(FFLAGS) $(FOPT) -c src/lab_2.f90 -I obj/ -o obj/lab_2.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/lab_2 obj/environment.o obj/source_process.o obj/source_io.o obj/lab_2.o

clean:
	rm -f obj/*
	rm -f bin/*

run:
	cd ./bin; ./lab_2
	cat ./bin/output.txt
