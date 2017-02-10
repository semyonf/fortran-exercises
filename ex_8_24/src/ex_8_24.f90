! 8.24 в учебнике

program ex_8_24
    implicit none
    integer, parameter      :: R_ = 4
    integer                 :: In = 0, Out = 0, N = 0, i = 0
    real(R_)                :: x, a, b, h, sum, integral, t
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) a, b
        read(In, *) N
        read(In, *) x
    close (In)

    if (mod(N,2) == 1 .or. N <= 0) then
        write(*,*) 'Число N должно быть положительным и четным. Отмена...'
        stop
    endif

    h = (b - a) / N
    t = a
    sum = F(t)

    i = 1
    do
        t = t + h
        sum = sum + 4 * F(t)

        i = i + 2

        if (i >= N) exit

        t = t + h
        sum = sum + 2 * F(t)
    enddo

    sum = sum + F(b)
    integral = h/3 * sum

    open (file=output_file, newunit=Out)
        write(Out,*) 'Ответ ', integral
    close (Out)

contains

real function F(x)
    implicit none
    real, intent(in) :: x

    F = (q(x) ** 2 + 1.57) / sqrt(q(x) + 2.15)

end function F

pure real function q(x)
    implicit none
    real, intent(in) :: x
    real             :: sum, old_E, new_E
    integer          :: i, j

    sum   = 1
    new_E = 1

    i   = 1
    do
        old_E = new_E
        new_E = old_E * x**i

        do j = i*2+1, 2, -1
            new_E = new_E / j
        enddo

        sum = sum + new_E
        if (new_E == old_E) exit
    enddo

    q = sum

end function q

end program ex_8_24
