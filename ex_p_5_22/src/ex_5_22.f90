! 5.22 в учебнике

program ex_5_22
    implicit none
    integer, parameter      :: R_ = 4
    integer                 :: Out = 0, In = 0, i = 0, N = 0
    real(R_)                :: h = .0, sum = .0, a = 0, b = 0, x = .0, integral = .0
    character(*), parameter :: output_file = "output.txt", &
                               input_file  = "../data/input.txt"

    open (file=input_file, newunit=In)
        read(In, *) a, b
        read(In, *) N
    close (In)

    if (mod(N,2) == 1 .or. N <= 0) then
        write(*,*) 'Число N должно быть положительным и четным. Отмена...'
        stop
    endif

    h = (b - a) / N
    x = a
    sum = F(x)

    i = 1
    do
        x = x + h
        sum = sum + 4 * F(x)

        i = i + 2

        if (i >= N) exit

        x = x + h
        sum = sum + 2 * F(x)
    enddo

    sum = sum + F(b)
    integral = h/3 * sum

    open (file=output_file, newunit=Out)
        write(Out,*) 'Интеграл ', integral
    close (Out)

    contains

    real function F(x)

        implicit none
        real :: x

        F = x ** 2

    end function F

end program ex_5_22