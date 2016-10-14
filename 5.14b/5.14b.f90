! 5.14б в учебнике
! Найти отрицательные в массиве

program ex4
    implicit none
    integer, dimension(25) :: X
    integer, dimension(25) :: negativeIndexes
    integer :: i, k = 0

    X = (/1, 2, 3, 4, -5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 /)

    write(*,*) '--X--'
    call outputMatrix(X, size(X))

    do i = 1, size(X)
        if (X(i) < 0) then
            k = k + 1
            negativeIndexes(k) = i
        endif
    enddo


    if (k /= 0) then
        write(*,*) '--negative indexes found--'
        call outputMatrix(negativeIndexes, k)
    endif

end program ex4

subroutine outputMatrix(matrix, size)
    implicit none

    integer :: i, size
    integer, dimension(size) :: matrix
    character(10) :: convertedInt = ''
    character(1000) :: line = ''

    do i = 1, size
        write(convertedInt, '(I5)') matrix(i)
        write(line, *) trim(line)//trim(convertedInt)
    enddo

    write(*,*) trim(line)

    line = ''

end subroutine outputMatrix