! 3.20 в учебнике

program ex_3_20
    implicit none
    integer, parameter :: arrayX = 2, arrayY = 3
    integer, dimension(arrayX, arrayY) :: A
    integer, dimension(arrayX * arrayY) :: B, C
    integer :: i, j, k = 1

    A = reshape((/ 1, 2, 3, 4, 5, 6 /), shape(A))

    do j = 1, arrayY
        do i = 1, arrayX
            B(k) = A(i,j)
            C(k) = A(j,i)
            k = k + 1
        enddo
    enddo

end program ex_3_20