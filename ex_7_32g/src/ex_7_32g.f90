! 7.32g в учебнике
! Упорядочить элементы в СТРОКАХ матрицы B(10,15) так, чтобы |B(i,j)| <= |B(i,(j+1))|

program ex_7_32g
    implicit none
    integer, allocatable    :: input(:), B(:,:), line(:)
    integer                 :: In, Out, x, y, i, t, j

    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt", &
                               E_ = "UTF-8"

    open (file=input_file, encoding=E_, newunit=In)
            read(In,'(I3)') x
            read(In,'(I3)') y

            allocate(input(x * y))
            allocate(B(y, x))
            allocate(line(x))

            read(In,'(I3)') input(:)
    close (In)

    B = reshape(input, shape(B))

    write(*, *) 'Input'
    do i = 1, y
            write(Out, *) B(i,:)
        enddo

    do i = 1, y
        line = B(i,:)
        do j = 1, x-1
            if (abs(line(j)) > abs(line(j+1))) then
                t = line(j)
                line(j)=line(j+1)
                line(j+1)=t
            endif
        enddo
        B(i,:) = line
    enddo

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out, *) 'Output'
        do i = 1, y
            write(Out, *) B(i,:)
        enddo
    close (Out)

end program ex_7_32g
