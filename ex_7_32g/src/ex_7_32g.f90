! 7.32g в учебнике
! Упорядочить элементы в СТРОКАХ матрицы по модулю по возрастанию

program ex_7_32g
    implicit none

    integer, allocatable    :: B(:,:), line(:)
    integer                 :: In, Out, x, y, i, j, t, k

    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y
        allocate(B(x,y))
        read(In, *) (B(:,i), i = 1, y)
    close (In)

    do j = 1, y
        line = B(:,j)
        do i = 1, x
            do k = i + 1, x
                if (abs(line(i)) > abs(line(k))) then
                    t = line(k)
                    line(k) = line(i)
                    line(i) = t
                endif
            enddo
        enddo
        B(:,j) = line
    enddo

    open (file=output_file, newunit=Out)
        write(Out, *) (B(:,j), j = 1, y)
    close (Out)

end program ex_7_32g
