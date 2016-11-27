! 7.1a в учебнике

program ex_7_1a
    implicit none

    integer, allocatable    :: A(:)
    integer                 :: In, Out, size, i
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) size
        allocate(A(size))
        read(In, *) A(:)
    close (In)

    do i = 1, size
        A(i:) = cshift(A(i:), maxloc(A(i:), 1) - 1)
    enddo

    open (file=output_file, newunit=Out)
        write(Out, *) A
    close (Out)

end program ex_7_1a