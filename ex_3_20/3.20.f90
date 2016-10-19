! 3.20 в учебнике

program ex3
    implicit none
    integer, parameter :: arrayX = 3, arrayY = 3
    integer, dimension(arrayX, arrayY) :: A
    integer, dimension(arrayX * arrayY) :: B, C
    integer :: i, j, k = 1

    A = reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), shape(A))

    write(*,*) '--array A--'
    call outputMatrix2D(A, arrayX, arrayY)

    do j = 1, arrayY
        do i = 1, arrayX
            B(k) = A(i,j)
            C(k) = A(j,i)
            k = k + 1
        enddo
    enddo

    write(*,*) '--array B--'
    call outputMatrix(B, size(B))

    write(*,*) '--array C--'
    call outputMatrix(C, size(C))

end program ex3

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