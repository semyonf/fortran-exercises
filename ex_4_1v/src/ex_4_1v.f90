! 4.1(в) в учебнике

program ex_4_1v
    implicit none

    integer, parameter      :: R_ = 4
    real(R_)                :: min, max, delta
    real(R_), allocatable   :: X(:), F(:)
    integer                 :: Out = 0, In = 0, i = 0, N = 0
    character(*), parameter :: input_file = "../data/input.txt", &
                               output_file = "output.txt"

    open (file=input_file, newunit=In)
        read(In, *) min, max, delta
    close (In)

    N = Int((max - min) / delta + .5) + 1

    allocate(X(N), F(N))

    forall (i = 1:N) &
        X(i) = min + i * delta

    F = (X ** 2) * (tan(X) + (sin(X) / X))

    open (file=output_file, newunit=Out)
        write (Out, '("  X   |   f")')
        write (Out, '(f0.4, T7, "| ", f0.4)') (X(i), F(i), i = 1, N)
    close (Out)

end program ex_4_1v