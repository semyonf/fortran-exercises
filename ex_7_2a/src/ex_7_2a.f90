! 7.2a в учебнике

program ex_7_2a
    implicit none

    integer                 :: size = 0, negatives = 0, Out = 0, In = 0, i = 0, j = 0, t = 0
    integer, allocatable    :: Arr(:)
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
    Arr = [pack(Arr, mask), pack(Arr, .not. mask)]

    ! Сортировка
    do i = 1, negatives
        do j = 1, i
            if (Arr(j) > Arr(j+1)) then
                t = Arr(j)
                Arr(j) = Arr(j+1)
                Arr(j+1) = t
            endif
        enddo
    enddo

    open (file=output_file, newunit=Out)
        write(Out, *) Arr(:)
    close (Out)
    
end program ex_7_2a