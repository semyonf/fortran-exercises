! 5.14б в учебнике
! Найти отрицательные индексы в массиве

program ex_5_14b
    implicit none
    integer                 :: arrayLength, Out = 0, In = 0, i = 0 
    integer, allocatable    :: X(:), Indexes(:), NegativeIndexes(:)
    logical, allocatable    :: mask(:)
    character(*), parameter :: output_file = "output.txt", &
                               input_file  = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) arrayLength
        allocate(X(arrayLength))
        read(In, *) X(:)
    close (In)

    Indexes = [(i, i = 1, arrayLength)]
    mask = X < 0

    NegativeIndexes = pack(Indexes, mask)

    open (file=output_file, newunit=Out)
        write(Out,*) 'Negative indexes'
        write(Out,*) NegativeIndexes
    close (Out)

end program ex_5_14b
