! 7.17a в учебнике

program ex_7_17a
    implicit none

    integer, allocatable    :: A(:,:), Inline(:), Indexes(:,:), k(:)
    logical, allocatable    :: mask(:)
    integer                 :: In, Out, x, y, i, max, t, j, pos

    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y
        allocate(A(x,y))
        read(In, *) (A(:,i), i = 1, y)
    close (In)

    max = maxval(A)

    Inline = reshape(A, [x*y])
    mask = Inline == max

    pos = count(mask)

    k = pack([(i,i=1,x*y)], mask)
    allocate(Indexes(pos,2))

    do t = 1, pos

        i = mod(k(t),x)
        j = k(t) / x

        if (i == 0) then
            i = x
        else
            j = j + 1
        endif

        Indexes(t,:) = [i, j]
    enddo

    open (file=output_file, newunit=Out)
        write(Out, *) 'Наибольший элемент:', max
        write(Out, *) 'X, Y координаты элемента(ов)'
        write(Out, '(I2,A1,I2)') (Indexes(i,1), ',' ,Indexes(i,2), i = 1, pos)
    close (Out)

end program ex_7_17a
