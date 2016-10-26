! 7.32g в учебнике
! Упорядочить элементы в СТРОКАХ матрицы B(10,15) так, чтобы |B(i,j)| <= |B(i,(j+1))|

program ex_8_24
    implicit none

    integer                 :: B(2,3), i, j, In, Out
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt", &
                               E_ = "UTF-8"
                               
contains

pure real function q(x)
    implicit none
    real, intent(in) :: x
    real             :: sum, current
    integer          :: i, k

    i = 0
    k = 1

    do
        current = (x ** i) / fac(k)
        sum = sum + current
        if (current .LE. (10 ** (-5))) then
            exit
        endif
        i = i + 1
        k = k + 2
    enddo

    q = sum
    
end function q

pure integer function fac(number)
    implicit none

    integer, intent(in) :: number
    integer             :: n, i

    n = 1
    do i = 1, number
        n = n * i
    enddo
    fac = n

end function fac

end program ex_8_24