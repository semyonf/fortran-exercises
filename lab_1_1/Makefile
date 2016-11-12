FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto
FOPT=-O3 -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) $(FOPT) -c src/reference_lab_1_1.f90 -I obj/ -o obj/reference_lab_1_1.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/reference_lab_1_1 ./obj/environment.o obj/reference_lab_1_1.o

clean:
	rm -f obj/*
	rm -f bin/*

run:
	cd ./bin; ./reference_lab_1_1
	cat ./bin/output.txt
