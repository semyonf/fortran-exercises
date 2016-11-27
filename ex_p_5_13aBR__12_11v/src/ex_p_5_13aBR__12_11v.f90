! Найти наибольший из отрицательных
! Зафиксировать элемент удовлетворяющий условию (ближайший к первому индекс, если несколько)
! Поменять местами найденный элемент с последним

program ex_p_5_13aBR__12_11v
    implicit none

    integer, parameter      :: R_ = 4
    real(R_), allocatable   :: Y(:)
    real(R_)                :: MaxNegVal
    integer                 :: In, Out, size, MaxNegLoc, i
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) size
        allocate(Y(size))
        read(In, *) Y(:)
    close (In)

    MaxNegVal = maxval(pack(Y, Y < 0))
    do i = 1, size
        if (Y(i) == MaxNegVal) then
            MaxNegLoc = i
            exit
        endif
    enddo

    Y(MaxNegLoc) = Y(size)
    Y(size) = MaxNegVal

    open (file=output_file, newunit=Out)
        write(Out, *) Y
    close (Out)

end program ex_p_5_13aBR__12_11v