! 5.24 в учебнике

program ex_5_24
    implicit none
    integer                 :: Out = 0, In = 0, i = 0, N = 0, M = 0, occurrences = 0
    integer, allocatable    :: A(:), B(:)
    character(*), parameter :: output_file = "output.txt", &
                               input_file  = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) N, M
        allocate(A(N), B(M))
        read(In, *) A(:)
        read(In, *) B(:)
    close (In)

    do concurrent (i = 1:N-M+1)
        if (All(A(i:i+M-1) == B)) occurrences = occurrences + 1
    enddo

    open (file=output_file, newunit=Out)
        write(Out,*) 'Появлений:', occurrences
    close (Out)

end program ex_5_24