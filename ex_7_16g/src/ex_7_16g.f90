! 7.16g в учебнике

program ex_7_16g
    implicit none

    integer                 :: x, y, Out = 0, In = 0
    integer, allocatable    :: input(:), B(:,:)
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt", &
                               E_ = "UTF-8"

    open (file=input_file, encoding=E_, newunit=In)
        read(In, '(I2)') x
        read(In, '(I2)') y
        allocate(input(x * y))
        read(In, '(I3)') input(:)
    close (In)

    allocate(B(x,y))

    B = reshape(input, [x, y])

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out, *) B(1,:)
        write(Out, *) B(2,:)
        write(Out, *) B(3,:)
        write(Out, *) B(4,:)
        write(Out, *) B(5,:)
    close (Out)
end program ex_7_16g