! 7.2a в учебнике

program ex_7_2a
    implicit none

    integer                 :: size = 0, negatives = 0, Out = 0, In = 0, i = 0, j = 0, t = 0
    integer, allocatable    :: Neg(:), Pos(:), Arr(:), Res(:)
    logical, allocatable    :: mask(:)
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) size
        allocate(Arr(size))
        read(In, *) Arr(:)
    close (In)

    mask = Arr <= 0
    negatives = count(mask)

    Neg = pack(Arr, mask)
    Pos = pack(Arr, .NOT. mask)

    ! Сортировка
    do i = 1, negatives
        do j = 1, i
            if (Neg(j) > Neg(j+1)) then
                t = Neg(j)
                Neg(j) = Neg(j+1)
                Neg(j+1) = t
            endif
        enddo
    enddo

    Res = [pack(Neg, .true.), pack(Pos, .true.)]

    open (file=output_file, newunit=Out)
        write(Out, *) Res(:)
    close (Out)
    
end program ex_7_2a