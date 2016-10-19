! 1.2 в учебнике

program ex1
    implicit none

    real :: a, b, c, d, e, f, x, p

    write(*,*) 'Enter a b c d e f x'
    read(*,*) a, b, c, d, e, f
    p = a * x ** 5 + b*x**4 + c*x**3 + d*x**2 + e*x + f
    write(*,*) p
end program ex1