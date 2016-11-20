! 5.22 в учебнике

real function F(x)
    implicit none
    real :: x
    F = x ** 2
end function F

program ex_5_22
    implicit none
    integer, parameter      :: R_ = 4
    integer                 :: Out = 0, In = 0, i = 0, N = 0
    real(R_)                :: h = .0, s = .0, t = .0, a = 0, b = 0, F
    character(*), parameter :: output_file = "output.txt", &
                               input_file  = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) a, b
        read(In, *) N
    close (In)

    h = (b - a) / real(N)
    s = 0

    t = a + h

    do i = 1, N
        s = s + F(a) + 4 * F(a + h/2) + f(t)
        a = a + h
        t = t + h

    enddo

    s = h / 6 * s

    open (file=output_file, newunit=Out)
        write(Out,*) 'Интеграл ', s
    close (Out)

end program ex_5_22