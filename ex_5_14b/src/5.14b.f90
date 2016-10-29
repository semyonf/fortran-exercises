! 5.14б в учебнике
! Найти отрицательные индексы в массиве

program ex_5_14b
    implicit none
    integer                 :: arrayLength, Out = 0, In = 0
    integer, allocatable    :: X(:), NegativeIndexes(:)
    character(*), parameter :: output_file = "output.txt", &
                               input_file  = "../data/input.txt", &
                                        E_ = "UTF-8"

    open (file=input_file, encoding=E_, newunit=In)
        read(In, '(I2)') arrayLength

        allocate(X(arrayLength))
        allocate(NegativeIndexes(arrayLength))

        read(In,'(I3)') X(:)
    close (In)

    NegativeIndexes = pack(X, X < 0)

    open (file=output_file, encoding=E_, newunit=Out)
        write(*,*) 'X'
        write(*,*) X

        write(*,*) 'Negative'
        write(*,*) NegativeIndexes
    close (Out)

end program ex_5_14b
