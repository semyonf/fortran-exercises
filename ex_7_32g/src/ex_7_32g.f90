! 7.32g в учебнике
! Упорядочить элементы в СТРОКАХ матрицы по модулю по возрастанию

program ex_7_32g
    implicit none

    integer, allocatable    :: B(:,:), AbsLine(:), Order(:)
    integer                 :: In, Out, x, y, i, j

    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y
        allocate(B(x,y), Order(x))
        read(In, *) (B(:,i), i = 1, y)
    close (In)

    do concurrent (j = 1:y)
        AbsLine = abs(B(:,j))
        Order = [(i,i = 1, x)]
        do i = 1, x
            Order(i:) = cshift(Order(i:), minloc(AbsLine(i:), 1) - 1)
            AbsLine = cshift(AbsLine, minloc(AbsLine(i:), 1) - 1)
        enddo
        B(:,j) = B(order,j)
    end do

    open (file=output_file, newunit=Out)
        write(Out, *) (B(:,j), j = 1, y)
    close (Out)

end program ex_7_32g