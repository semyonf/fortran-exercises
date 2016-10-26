! 7.2a в учебнике

program ex_7_2a
    implicit none

    integer                 :: Out = 0, In = 0, Arr(75), Res(75)
    integer, allocatable    :: Neg(:), Pos(:)
    logical                 :: mask(75)
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt", &
                               E_ = "UTF-8"

    open (file=input_file, encoding=E_, newunit=In)
        read(In, '(75I3)') Arr
    close (In)

    mask = Arr .LE. 0
    allocate(Neg(count(mask)))
    allocate(Pos(size(Arr) - count(mask)))
    
    Neg = pack(Arr, mask)
    Pos = pack(Arr, .NOT. mask)

    Res(1:) = Neg
    Res(size(Neg) + 1:) = Pos

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out, '(I3)') Res
    close (Out)
end program ex_7_2a