! 7.32g в учебнике
! Упорядочить элементы в СТРОКАХ матрицы по модулю по возрастанию

program ex_7_32g
    implicit none

    integer, allocatable    :: B(:,:), Line(:), Order(:)
    integer                 :: In, Out, x, y, i, j, shiftAmount

    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y
        allocate(B(x,y))
        allocate(Order(x))
        read(In, *) (B(:,i), i = 1, y)
    close (In)

    do concurrent (j = 1:y)
        Line = abs(B(:,j))
        Order = [(i,i = 1, x)]
        do i = 1, x
            shiftAmount = minloc(Line(i:), 1) - 1
            Line(i:)  = cshift(Line(i:), shiftAmount)
            Order(i:) = cshift(Order(i:), shiftAmount)
        enddo
        B(:,j) = B(order,j)
    end do

    open (file=output_file, newunit=Out)
        write(Out, *) (B(:,j), j = 1, y)
    close (Out)

end program ex_7_32g