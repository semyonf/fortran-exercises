! 2.3 в учебнике

program ex2
    implicit none

    integer(1), parameter :: quantity = 4
    integer(1) :: positive = 0, i
    character(1) :: vars(quantity)
    real :: input

    vars = (/'a', 'b', 'c', 'd'/)

    do i = 1, quantity
       write(*,*) 'Enter variable ', vars(i)
       read(*,*) input;
       if (input > 0) then
           positive = positive + 1
       endif
    end do

    write(*,*) positive, ' positive number(s) were entered'
end program ex2