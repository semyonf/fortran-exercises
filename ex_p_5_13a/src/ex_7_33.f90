! 7.33 в учебнике

program ex_7_33
    implicit none

    integer, allocatable    :: B(:,:), largest(:), indexes(:)
    integer                 :: In, Out, x, y, i

    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y
        allocate(B(x,y), largest(y), indexes(y))
        read(In, *) (B(i,:), i = 1, y)
    close (In)

    largest = [(maxval(B(i,:)), i = 1, y)]
    indexes = [(i, i = 1, y)]

    do i = 1, x
        indexes(i:) = cshift(indexes(i:), maxloc(largest(i:), 1) - 1)
        largest(i:) = cshift(largest(i:), maxloc(largest(i:), 1) - 1)
    enddo

    B = B(indexes,:)

    open (file=output_file, newunit=Out)
        write(Out, *) (B(i,:), i = 1, y)
    close (Out)

end program ex_7_33