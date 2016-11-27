! 7.17a в учебнике

program ex_7_17a
    implicit none

    integer, allocatable    :: A(:,:)
    integer                 :: In, Out, x, y, i

    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y
        allocate(A(x,y))
        read(In, *) (A(i,:), i = 1, y)
    close (In)

    open (file=output_file, newunit=Out)
        write(Out, *) (A(i,:), i = 1, y)
        write(Out, *) 'Наибольший элемент:', maxval(A)
    close (Out)

end program ex_7_17a