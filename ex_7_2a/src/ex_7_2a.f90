! 7.2a в учебнике

program ex_7_2a
    implicit none

    integer                 :: size = 0, amountOfNegatives = 0, Out = 0, In = 0, i = 0, j = 0, t = 0
    integer, allocatable    :: Neg(:), Pos(:), Arr(:), Res(:)
    logical, allocatable    :: mask(:)
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt", &
                               E_ = "UTF-8"

    open (file=input_file, encoding=E_, newunit=In)
        read(In, '(I2)') size
        allocate(Arr(size))
        allocate(Res(size))
        allocate(mask(size))
        read(In, '(I3)') Arr(:)
    close (In)

    mask = Arr <= 0
    amountOfNegatives = count(mask)
    allocate(Neg(amountOfNegatives))
    allocate(Pos(size - amountOfNegatives))

    Neg = pack(Arr, mask)
    Pos = pack(Arr, .NOT. mask)

    do i = 0, amountOfNegatives
        do j = 1, i
            if (Neg(j) > Neg(j+1)) then
                t = Neg(j)
                Neg(j) = Neg(j+1)
                Neg(j+1) = t
            endif
        enddo
    enddo

    Res = [pack(Neg, .TRUE.), pack(Pos, .TRUE.)]

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out, '(I3)') Res
    close (Out)
    
end program ex_7_2a