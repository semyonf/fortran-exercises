FC=gfortran
FFLAGS=-Wall -std=f2008ts -fimplicit-none -Wno-maybe-uninitialized -static-libgfortran -flto
FOPT=-O3 -fopt-info-vec

all:
	$(FC) $(FFLAGS) -c src/environment.f90 -J obj/ -o obj/environment.o
	$(FC) $(FFLAGS) -c src/emp_io.f90 -J obj/ -o obj/emp_io.o
	$(FC) $(FFLAGS) -c src/emp_process.f90 -J obj/ -o obj/emp_process.o
	$(FC) $(FFLAGS) $(FOPT) -c src/lab_1_4.f90 -I obj/ -o obj/lab_1_4.o
	$(FC) $(FFLAGS) $(FOPT) -o bin/lab_1_4 obj/environment.o obj/emp_process.o obj/emp_io.o obj/lab_1_4.o

clean:
	rm -f obj/*
	rm -f bin/*

run:
	cd ./bin; ./lab_1_4
	cat ./bin/output.txt
