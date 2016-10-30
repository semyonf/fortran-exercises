! 4.1(в) в учебнике

program ex_4_1v
    implicit none


    integer, parameter      :: R_ = 4
    character(*), parameter :: input_file = "../data/input.txt", &
                               output_file = "output.txt", &
                               E_ = "UTF-8"

    real(R_)                :: min, max, delta
    real(R_), allocatable   :: ArrX(:), ArrF(:)

    integer                 :: Out = 0, In = 0, i = 0, N = 0

    open (file=input_file, encoding=E_, newunit=In)
        read(In, '(f7.5)') min
        read(In, '(f7.5)') max
        read(In, '(f7.5)') delta
    close (In)

    N = Int((max-min)/delta)

    allocate(ArrX(N), ArrF(N))

    forall (i=1:N)
        ArrF(i) = (ArrX(i) ** 2) * (tan(ArrX(i))) + (sin(ArrX(i))) / (ArrX(i))
    end forall

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out,'(f7.5)') ArrF(:)
    close (Out)

end program ex_4_1v