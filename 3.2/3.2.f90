! 3.20 в учебнике

program ex3
    implicit none
    integer, parameter :: arrayX = 3, arrayY = 3
    real, dimension(arrayX, arrayY) :: A
    real, dimension(arrayX * arrayY) :: B, C
    integer :: i, j, k = 1

    ! A = reshape((/ 1, 2, 3, 4, 5, 6, 7, 8, 9 /), shape(A))
    
    CALL RANDOM_NUMBER(A)

    write(*,*) '--A--'

    do j = 1, arrayY
        do i = 1, arrayX
            write(*,'(5F0.2)') A(i,j)
            B(k) = A(i,j)
            C(k) = A(j,i)
            k = k + 1
        enddo
        write(*,*)
    enddo

    write(*,*)
    write(*,*) '--B--'

    do i = 1, arrayX * arrayY
        write(*,'(5F0.2)') B(i)
    enddo

    write(*,*)

    write(*,*) '--C--'

    do i = 1, arrayX * arrayY
        write(*,'(5F0.2)') C(i)
    enddo


end program ex3