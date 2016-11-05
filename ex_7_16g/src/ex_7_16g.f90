! 7.16g в учебнике

program ex_7_16g
    implicit none

    integer                 :: x, y, Out = 0, In = 0, i = 0, maxCol = 0, min = 0
    integer, allocatable    :: B(:,:), colSum(:)
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y
        allocate(B(x,y))
        read(In, *) (B(:,i), i = 1, y)
    close (In)

    colSum = [([sum(B(i,:))], i = 1, y)]
    maxCol = maxloc(colSum, 1)
    min = minval(B(maxCol,:))

    open (file=output_file, newunit=Out)
        write(Out, *) min

        ! Однострочная версия
        ! write(*,*) minval(B(maxloc([([sum(B(i,:))], i = 1, y)], 1),:))
    close (Out)
end program ex_7_16g