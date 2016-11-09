! 7.53

program ex_7_53
    implicit none

    integer, allocatable    :: Z(:,:), Y(:,:), BIG(:,:), SMALL(:,:), test(:,:)
    logical, allocatable    :: Rmask(:,:)
    integer                 :: In, Out, N_Z, N_Y, i, b_Size
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) N_Z
        allocate(Z(N_Z,N_Z))
        read(In, *) (Z(:,i), i = 1, N_Z)

        read(In, *) N_Y
        allocate(Y(N_Y,N_Y))
        read(In, *) (Y(:,i), i = 1, N_Y)
    close (In)

    if (maxval([N_Y, N_Z]) == N_Y) then
        b_Size = N_Y
        BIG = Y
        SMALL = Z
    else
        b_Size = N_Z
        BIG = Z
        SMALL = Y
    endif

    Rmask = (SMALL /= 0 .and. BIG /= 0)

    allocate(test(3,3))

    test = reshape(pack(SMALL, Rmask), [3,3], [0,0,0,0,0,0,0,0,0,0,0,0])

    open (file=output_file, newunit=Out)
        write(Out, *) '-y-'
        write(Out, *) BIG(:,1)
        write(Out, *) BIG(:,2)
        write(Out, *) BIG(:,3)
        write(Out, *) BIG(:,4)
        write(Out, *) '-z-'
        write(Out, *) SMALL(:,1)
        write(Out, *) SMALL(:,2)
        write(Out, *) SMALL(:,3)
        write(Out, *) '-r-'
        write(Out, *) Rmask(:,1)
        write(Out, *) Rmask(:,2)
        write(Out, *) Rmask(:,3)
        write(Out, *) '-test-'
        write(Out, *) test(:,1)
        write(Out, *) test(:,2)
        write(Out, *) test(:,3)
        ! write(Out, *) (Z(:,i), i = 1, N_Z)
        ! write(Out, *) (Y(:,i), i = 1, N_Y)
    close (Out)

end program ex_7_53