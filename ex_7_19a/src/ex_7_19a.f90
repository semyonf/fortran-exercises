! 7.19a в учебнике
! количесвто элементов матрицы B, которые положительны
! вывести их индексы

! TODO надо сделать херню для индексов массива

program ex_7_19a
    implicit none
    integer, allocatable    :: B(:,:), Indexes(:,:), NegativeIndexes(:)
    logical, allocatable    :: mask(:,:)
    integer                 :: x = 0, y = 0, In = 1, Out = 0, i = 0, j = 0, positives = 0
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y

        allocate(B(x,y), Indexes(x,y))

        read(In, *) (B(:,i), i = 1, y)
    close (In)

    mask = B > 0
    positives = count(mask)

    allocate(NegativeIndexes(x*y - positives))

    do i = 1, y
        do j = 1,x
            Indexes(j,i) = i*10 + j
        enddo
    enddo

    NegativeIndexes = pack(Indexes, .NOT. mask)

    open (file=output_file, newunit=Out)
        write(Out, *) positives, 'positive elements found within the array'
        write(Out, *) 'Negative indexes:'
        write(Out, *) NegativeIndexes
    close (In)

end program ex_7_19a
