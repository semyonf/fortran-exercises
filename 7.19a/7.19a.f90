! 7.19a в учебнике
! количесвто элементов матрицы B, кторые положительны

program ex5
    implicit none
    integer, dimension(5, 5) :: B
    integer :: i, j, positiveElements = 0

    B = reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, -9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25/), shape(B))

    write(*,*) '--B--'

    do j = 1, 5
        do i = 1, 5
            ! write(*,'(5F0.2)') B(i,j)
            if (B(i,j) > 0) then
                positiveElements = positiveElements + 1
            endif
        enddo
    enddo

    call outputMatrix2D(B, 5, 5)

    write(*,*) 'Amount of positive elems: ', positiveElements

end program ex5

subroutine outputMatrix2D(matrix, X, Y)
    implicit none

    integer :: X, Y, i, j
    integer, dimension(X, Y) :: matrix
    character(10) :: convertedInt = ''
    character(1000) :: line = ''

    do j = 1, Y
        do i = 1, X
            write(convertedInt, '(I5)') matrix(i,j)
            write(line, *) trim(line)//trim(convertedInt)
        enddo
        write(*,'(a20)') trim(line)
        line = ''
    enddo

    line = ''

end subroutine outputMatrix2D