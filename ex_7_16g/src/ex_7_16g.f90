! 7.16g в учебнике

program ex_7_16g
    implicit none

    integer                 :: x, y, Out = 0, In = 0, i = 0, maxCol = 0, min = 0
    integer, allocatable    :: B(:,:)
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y
        allocate(B(x,y))
        read(In, *) (B(:,i), i = 1, y)
    close (In)

    maxCol = maxloc(sum(B, 2), 1)
    min = minval(B(maxCol,:))

    open (file=output_file, newunit=Out)
        write(Out, *) min
    close (Out)
end program ex_7_16g