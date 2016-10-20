! 7.32(г) в учебнике
! Упорядочить элементы в СТРОКАХ матрицы B(10,15) так, чтобы |B(i,j)| <= |B(i,(j+1))|

program ex_7_32g
    implicit none

    integer, parameter      :: x = 4, y = 5
    integer                 :: B(x, y)
    logical                 :: mask(x, y)
    integer                 :: i, j, positiveElements = 0, Out = 0, In = 0
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt", &
                               E_ = "UTF-8"

    open (file=input_file, encoding=E_, newunit=In)
        do i = 1, y
            read(In,"(10g3.5)") (B(j, i), j = 1, x)
        enddo
    close (In)

    mask = B .GE. 0
    positiveElements = count(mask)

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out,*) 'B'
        do i = 1, y
            write(Out,"(10g3.5)") (B(j, i), j = 1, x)
        enddo
        write(Out,*)
        write(Out,*) 'Positive elements:', positiveElements
    close (Out)
end program ex_7_32g