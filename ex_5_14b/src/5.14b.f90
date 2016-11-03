! 5.14б в учебнике
! Найти отрицательные индексы в массиве

program ex_5_14b
    implicit none
    integer                 :: arrayLength, Out = 0, In = 0, i = 0 
    integer, allocatable    :: X(:), Indexes(:), NegativeIndexes(:)
    logical, allocatable    :: mask(:)
    character(*), parameter :: output_file = "output.txt", &
                               input_file  = "../data/input.txt", &
                                        E_ = "UTF-8"

    open (file=input_file, encoding=E_, newunit=In)
        read(In, '(I2)') arrayLength

        allocate(X(arrayLength))
        
        read(In,'(I3)') X(:)
    close (In)

    allocate(Indexes(arrayLength))
    do i = 1, arrayLength
        Indexes(i) = i
    enddo

    allocate(mask(arrayLength))
    mask = X < 0

    allocate(NegativeIndexes(count(mask)))
    NegativeIndexes = pack(Indexes, mask)

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out,*) 'X'
        write(Out,*) X

        write(Out,*) 'Negative'
        write(Out,*) NegativeIndexes
    close (Out)

end program ex_5_14b
