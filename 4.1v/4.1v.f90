! 4.1(в) в учебнике

program exxxx
    implicit none
    real :: x = .01254, delta = .0002, f

    do while (x .LE. .03254)
        f = x**2 * TAN(x) + (SIN(x)/x)
        write(*,*) f
        x = x + delta
    end do

end program exxxx