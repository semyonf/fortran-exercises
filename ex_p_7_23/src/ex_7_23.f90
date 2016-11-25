! 7.23 в учебнике

program ex_7_23
    implicit none

    integer, allocatable    :: B(:,:)
    integer                 :: In, Out, x, y, i

    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y
        allocate(B(x,y))
        read(In, *) (B(i,:), i = 1, y)
    close (In)

    open (file=output_file, newunit=Out)
        write(Out, *) (B(i,:), i = 1, y)
    close (Out)

end program ex_7_23