! 7.32g в учебнике
! Упорядочить элементы в СТРОКАХ матрицы B(10,15) так, чтобы |B(i,j)| <= |B(i,(j+1))|

program ex_7_32g
    implicit none

    integer                 :: B(4,5), i, j, k, In, Out, line(4), t
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt", &
                               E_ = "UTF-8"

    open (file=input_file, encoding=E_, newunit=In)
        do i = 1, 5
            read(In,"(4i3)") B(:, i)
        enddo
    close (In)

    B = reshape((B), shape(B))

    do k = 1, 5
        line = B(:, k)

        do i = size(line)-1, 1, -1
            do j = 1, i
                if (abs(line(j)) .gt. abs(line(j+1))) then
                    t=line(j)
                    line(j)=line(j+1)
                    line(j+1)=t
                endif
            enddo
        enddo

        ! write(*, *) line
        B(:, k) = line
    enddo


    open (file=output_file, encoding=E_, newunit=Out)
        20 format(5(/,4i3))
        write(Out, 20) B
    close (Out)

end program ex_7_32g