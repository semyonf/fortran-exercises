! 7.53

program ex_7_53
    implicit none

    integer, allocatable    :: Z(:,:), Y(:,:), test(:,:)
    logical, allocatable    :: Ymask(:,:), Zmask(:,:), Rmask(:,:)
    integer                 :: In, Out, N_Z, N_Y, i
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

    allocate(test(maxval([N_Y, N_Z]),maxval([N_Y, N_Z])))

    Ymask = (Y /= 0)
    Zmask = (Z /= 0)

    if (maxval([N_Y, N_Z]) == N_Y) then
        Rmask = (Zmask .and. Ymask)
    else
        Rmask = (Ymask .and. Zmask)
    endif

    

    open (file=output_file, newunit=Out)
        write(Out, *) '-y-'
        write(Out, *) Ymask(:,1)
        write(Out, *) Ymask(:,2)
        write(Out, *) Ymask(:,3)
        write(Out, *) Ymask(:,4)
        write(Out, *) '-z-'
        write(Out, *) Zmask(:,1)
        write(Out, *) Zmask(:,2)
        write(Out, *) Zmask(:,3)
        write(Out, *) '-r-'
        write(Out, *) Rmask(:,1)
        write(Out, *) Rmask(:,2)
        write(Out, *) Rmask(:,3)
        ! write(Out, *) (Z(:,i), i = 1, N_Z)
        ! write(Out, *) (Y(:,i), i = 1, N_Y)
    close (Out)

end program ex_7_53