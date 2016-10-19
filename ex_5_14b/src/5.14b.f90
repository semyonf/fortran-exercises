! 5.14б в учебнике
! Найти отрицательные индексы в массиве

program ex_5_14b
    implicit none
    integer, parameter     :: arrayLength = 5
    integer                :: X(arrayLength), i = 0, Out = 0
    character(*), parameter :: output_file = "output.txt", &
                               E_ = "UTF-8"

    X = (/1, 2, 3, 4, -5/)

    open (file=output_file, encoding=E_, newunit=Out)
        write(*,*) 'X'
        write(*,"(10g3.5)") (X(i), i = 1, arrayLength)

        write(*,*) 'Negative'
        write(*,*) pack(X, X < 0)
    close (Out)

end program ex_5_14b