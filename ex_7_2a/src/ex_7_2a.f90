! 7.2a в учебнике

program ex_7_2a
    implicit none

    integer                 :: size = 0, negatives = 0, Out = 0, In = 0, i = 0, minpos = 0
    integer, allocatable    :: Arr(:)
    logical, allocatable    :: mask(:)
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) size
        allocate(Arr(size))
        read(In, *) Arr(:)
    close (In)

    mask = Arr < 0

    negatives = count(mask)

    Arr = [pack(Arr, mask), pack(Arr, .not. mask)]

    do i = 1, negatives
        minpos = minloc(Arr(i:negatives), 1)
        if (minpos /= 1) then
            Arr(i:negatives) = cshift(Arr(i:negatives), minpos - 1)
        endif
    enddo

    open (file=output_file, newunit=Out)
        write(Out, *) Arr(:)
    close (Out)
    
end program ex_7_2a