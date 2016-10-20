! 3.20 в учебнике

program ex_3_20
    implicit none
    integer, parameter      :: arrayX = 3, arrayY = 2
    integer                 :: A(arrayX, arrayY)
    integer                 :: B(arrayX * arrayY), C(arrayX * arrayY)
    integer                 :: i, j, Out = 0, In = 0
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt", &
                               E_ = "UTF-8"

    23 format(2(/,3i3))
    6 format(6i3)

    open (file=input_file, encoding=E_, newunit=In)
        do i = 1, arrayY
            read(In,"(10g3.5)") (A(j, i), j = 1, arrayX)
        enddo
    close (In)

    B = reshape(A, shape(B))
    C = reshape(transpose(A), shape(C))

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out,*) 'A'
        write(Out,*)
        write(Out, 23) A
        write(Out,*) 'B'
        write(Out,*)
        write(Out, 6) B
        write(Out,*) 'C'
        write(Out,*)
        write(Out, 6) C
    close (Out)

end program ex_3_20