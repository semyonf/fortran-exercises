! 3.20 в учебнике

program ex_3_20
    implicit none
    integer                 :: arrayX, arrayY, Out = 0, In = 0
    integer, allocatable    :: input(:), A(:,:), B(:), C(:)
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt", &
                               E_ = "UTF-8"

    open (file=input_file, encoding=E_, newunit=In)
        read(In, '(1I3)') arrayX
        read(In, '(1I3)') arrayY

        allocate(input(arrayY * arrayX))

        allocate(A(arrayY, arrayX))
        allocate(B(arrayY * arrayX))
        allocate(C(arrayY * arrayX))

        read(In,'(I2)') input(:)

    close (In)

    A = reshape(input, [arrayX, arrayY])
    C = reshape(A, [arrayX * arrayY])
    A = reshape(input, [arrayX, arrayY], order=[2, 1])
    B = reshape(A, [arrayX * arrayY])

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out,*) 'A'
        write(Out,'(:(/,3i3))') A
        write(Out,*) 'B'
        write(Out,'(I3)') B(:)
        write(Out,*) 'C'
        write(Out,'(I3)') C(:)
    close (Out)

end program ex_3_20
