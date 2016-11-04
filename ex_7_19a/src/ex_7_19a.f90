! 7.19a в учебнике
! количесвто элементов матрицы B, которые положительны
! Корректно читать двумерный массив по строкам.
! "Count. Составить маску. Упаковать массив индексов (см. раздаточные 7.х)."

program ex_7_19a
    implicit none
    integer, allocatable    :: B(:,:), Indexes(:,:), NegativeIndexes(:)
    logical, allocatable    :: mask(:,:)
    integer                 :: x = 0, y = 0, In = 1, Out = 0, i = 0, j = 0, positives = 0
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"
    character(50)           :: fmt
    character(50)           :: temp

    open (file=input_file, newunit=In)
        read(In, *) x, y

        allocate(B(x,y), mask(x,y), Indexes(x,y))

        read(In, *) (B(:,i), i = 1, y)
    close (In)

    mask = B > 0
    positives = count(mask)

    allocate(NegativeIndexes(count(.NOT. mask)))

    do i = 1, y
        do j = 1,x
            Indexes(j,i) = i*10 + j
        enddo
    enddo

    NegativeIndexes = pack(Indexes, .NOT. mask)

    ! Array looks cleaner this way
    write(temp, *) x
    fmt = '(' // trim(temp) // 'I4)'

    open (file=output_file, newunit=Out)
        write(Out, *) 'Array B'
        write(Out, fmt) (B(:,i), i = 1, y)
        write(Out, *)
        write(Out, *) 'Array Indexes'
        write(Out, fmt) (Indexes(:,i), i = 1, y)
        write(Out, *)
        write(Out, *) NegativeIndexes, ' - negative indexes'
        write(Out, *)
        write(Out, *) positives, ' positive elements within array found'
    close (In)

end program ex_7_19a
