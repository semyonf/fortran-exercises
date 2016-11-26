! 7.23 в учебнике

program ex_7_23
    implicit none

    integer, allocatable    :: B(:,:), sums(:,:)
    integer                 :: In, Out, x, y, i, j, maximum

    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y
        allocate(B(x,y), sums(y-1,x-1))
        read(In, *) (B(i,:), i = 1, y)
    close (In)

    do i = 1, y - 1
        do j = 1, x - 1
            sums(i,j) = sum(B(i:i+1,j:j+1))
        enddo
    enddo

    maximum = maxval(sums)

    open (file=output_file, newunit=Out)
        write(Out,*) 'Максимальная сумма равна', maximum 
        write(Out,*) 'Координаты индексов:'
        do i = 1, y - 1
            do j = 1, x - 1
                if (sums(i,j) == maximum) then
                    write(Out,'(I2,a,I2)') i,',',j
                endif
            enddo
        enddo
    close (Out)

end program ex_7_23