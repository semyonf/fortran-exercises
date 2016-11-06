! 7.19a в учебнике
! количесвто элементов матрицы B, которые отрицательны
! вывести их индексы

! TODO надо сделать херню для вывода индексов массива

program ex_7_19a
    implicit none
    integer, allocatable    :: B(:,:), Inline(:), Indexes(:,:)
    logical, allocatable    :: mask(:)
    integer                 :: x = 0, y = 0, In = 1, Out = 0, i = 0, j = 0, pos = 0
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y
        allocate(B(x,y), Indexes(x*y,2))
        read(In, *) (B(:,i), i = 1, y)
    close (In)

    Inline = reshape(B, [x*y])

    mask = Inline < 0
    pos = count(mask)

    Indexes(:, 1) = [((i, i = 1, x), j = 1, y)]
    Indexes(:, 2) = [((j, i = 1, x), j = 1, y)]

    write(*, *) 'B'
    write(*, *) B
    write(*, *) 'Negative mask'
    write(*, *) mask
    write(*, *) 'Positions'
    write(*, *) pack([(i,i=1,x*y)], mask)
    write(*, *) 'X,Y'
    write(*, '(I2,A1,I2)') Indexes(11,1), ',' ,Indexes(11,2)
    write(*, '(I2,A1,I2)') Indexes(12,1), ',' ,Indexes(12,2)

    open (file=output_file, newunit=Out)
        write(Out, *) pos, 'negative elements found within the array'
        ! write(Out, *) 'Negative indexes:'

        ! write(Out, '(I2,A1,I2)') Indexes(1,1), ',' ,Indexes(1,2)
        ! write(Out, '(I2,A1,I2)') Indexes(2,1), ',' ,Indexes(2,2)
        ! write(Out, '(I2,A1,I2)') Indexes(3,1), ',' ,Indexes(3,2)
        ! write(Out, '(I2,A1,I2)') Indexes(4,1), ',' ,Indexes(4,2)
        ! write(Out, '(I2,A1,I2)') Indexes(5,1), ',' ,Indexes(5,2)
        ! write(Out, '(I2,A1,I2)') Indexes(6,1), ',' ,Indexes(6,2)
        ! write(Out, '(I2,A1,I2)') Indexes(7,1), ',' ,Indexes(7,2)
        ! write(Out, '(I2,A1,I2)') Indexes(8,1), ',' ,Indexes(8,2)
        ! write(Out, '(I2,A1,I2)') Indexes(9,1), ',' ,Indexes(9,2)
        ! write(Out, '(I2,A1,I2)') Indexes(10,1), ',' ,Indexes(10,2)
        ! write(Out, '(I2,A1,I2)') Indexes(11,1), ',' ,Indexes(11,2)
        ! write(Out, '(I2,A1,I2)') Indexes(12,1), ',' ,Indexes(12,2)
        ! write(Out, '(I2,A1,I2)') Indexes(13,1), ',' ,Indexes(13,2)
        ! write(Out, '(I2,A1,I2)') Indexes(14,1), ',' ,Indexes(14,2)
        ! write(Out, '(I2,A1,I2)') Indexes(15,1), ',' ,Indexes(15,2)
        ! write(Out, '(I2,A1,I2)') Indexes(16,1), ',' ,Indexes(16,2)
        ! write(Out, '(I2,A1,I2)') Indexes(17,1), ',' ,Indexes(17,2)
        ! write(Out, '(I2,A1,I2)') Indexes(18,1), ',' ,Indexes(18,2)
        ! write(Out, '(I2,A1,I2)') Indexes(19,1), ',' ,Indexes(19,2)
        ! write(Out, '(I2,A1,I2)') Indexes(20,1), ',' ,Indexes(20,2)
    close (In)

end program ex_7_19a
