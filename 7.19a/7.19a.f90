! 7.19a в учебнике
! количесвто элементов матрицы B, кторые положительны

program ex5
    implicit none
    real, dimension(15, 25) :: B
    integer :: i, j, positiveElements = 0

    CALL RANDOM_NUMBER(B)

    B(14,14) = -100

    write(*,*) '--B--'

    do j = 1, 25
        do i = 1, 15
            write(*,'(5F0.2)') B(i,j)
            if (B(i,j) > 0) then
                positiveElements = positiveElements + 1
            endif
        enddo
        write(*,*)
    enddo

    write(*,*) 'Amount of positive elems: ', positiveElements

end program ex5