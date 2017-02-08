! 7.19a в учебнике
! 1. Количесвто элементов матрицы B, которые положительны
! 2. Вывести их индексы

program ex_7_19a
    implicit none
    integer, allocatable    :: B(:,:), Inline(:), Indexes(:,:), k(:)
    logical, allocatable    :: mask(:)
    integer                 :: x = 0, y = 0, In = 1, i = 0, j = 0, t = 0, pos = 0, Out = 0
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y
        allocate(B(x,y))
        read(In, *) (B(:,i), i = 1, y)
    close (In)

    Inline = reshape(B, [x*y])
    mask = Inline > 0

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
        write(Out, *) pos, 'positive elements found within the array, coordinates:'
        write(Out, *) 'X, Y coordinates'
        write(Out, '(I2,A1,I2)') (Indexes(i,1), ',' ,Indexes(i,2), i = 1, pos)
    close (In)

end program ex_7_19a
