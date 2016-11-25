! 7.34 в учебнике

program ex_7_34
    implicit none

    integer, allocatable    :: B(:,:), max_vals(:), max_poss(:)
    integer                 :: In, Out, x = 0, y = 0, i = 0

    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) x, y
        allocate(B(x,y), max_vals(minval([x,y])), max_poss(y))
        read(In, *) (B(i,:), i = 1, y)
    close (In)

    max_vals = [(maxval(B(i,:)), i = 1, y)]
    max_poss = [(maxloc(B(i,:)), i = 1, y)]

    do i = 1, y
        B(i,max_poss(i)) = B(i,i)
        B(i,i) = max_vals(i)
    enddo

    open (file=output_file, newunit=Out)
        write(Out, *) (B(i,:), i = 1, y)
    close (Out)

end program ex_7_34