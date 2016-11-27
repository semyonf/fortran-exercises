! Найти наименьшее по модулю значение
! Зафиксировать элемент удовлетворяющий условию (ближайший к последнему индекс, если несколько)
! Поменять местами найденный элемент с последним

program ex_p_5_13aBR__12_11bBR
    implicit none

    integer, parameter      :: R_ = 4
    real(R_), allocatable   :: Y(:)
    real(R_)                :: AbsMinVal
    integer                 :: In, Out, size, i, AbsMinLoc
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) size
        allocate(Y(size))
        read(In, *) Y(:)
    close (In)

    AbsMinLoc = size - minloc(abs([(Y(size-i),i=0,size-1)]), 1) + 1
    AbsMinVal = Y(AbsMinLoc)

    Y(AbsMinLoc) = Y(size)
    Y(size) = AbsMinVal

    open (file=output_file, newunit=Out)
        write(Out, *) Y
    close (Out)

end program ex_p_5_13aBR__12_11bBR