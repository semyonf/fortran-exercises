! 4.14б в учебнике
! Найти отрицательные в массиве

program ex4
    implicit none
    real, dimension(25) :: X
    integer, dimension(25) :: negativeIndexes
    integer :: i, k = 1

    X = (/1, 2, 3, 4, -5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25 /)

    write(*,*) '--X--'

    do i = 1, 25
        write(*,*) X(i)
        if (X(i) < 0) then
            negativeIndexes(k) = i
            k = k + 1
        endif
    enddo


    if (k /= 1) then
        write(*,*) '--negative indexes found--'
        do i = 1, k - 1
            write(*,*) negativeIndexes(i)
        enddo
    endif

end program ex4