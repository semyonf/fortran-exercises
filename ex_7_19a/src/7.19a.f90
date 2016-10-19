! 7.19a в учебнике
! количесвто элементов матрицы B, которые положительны

program ex_7_19a
    implicit none

    integer, parameter      :: x = 4, y = 5
    integer                 :: B(x, y)
    logical                 :: mask(x, y)
    integer                 :: i, j, positiveElements = 0, Out = 0
    character(*), parameter :: output_file = "output.txt", &
                               E_ = "UTF-8"

    B = reshape((/ 1, 2, -3, 4, 5, 6, 7, 8, -9, 10, &
                   11, 0, -13, 14, 15, 16, -17, 18, 19, 20/), shape(B))

    mask = B .GE. 0

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out,*) 'B'
        do i = 1, y
            write(Out,"(10g3.5)") (B(j, i), j = 1, x)
        enddo
        write(Out,*)
        positiveElements = count(mask)
        write(Out,*) 'Positive elements:', positiveElements
    close (Out)

end program ex_7_19a