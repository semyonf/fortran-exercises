! 6.2e в учебнике

program ex_6_2e
    implicit none

    integer, parameter      :: R_ = 4
    real(R_), parameter     :: RELERR = 0.0005
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt", &
                               E_ = "UTF-8"
    real(R_)                :: x = 0, sum = 0, current = 0, a = 0
    integer                 :: In = 0, Out = 0, i = 0

    open (file=input_file, encoding=E_, newunit=In)
        read(In,'(1f5.2)') a
        read(In,'(1f5.2)') x
    close (In)

    do
        current = (x * log(a)) ** i / fac(i)
        sum = sum + current
        ! write(*,*) 'current = ', current, 'sum = ', sum
        if ((current / sum) .LT. RELERR) then
            exit
        endif
        i = i + 1
    end do

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out, *) sum
    close (Out)

contains

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

end program ex_6_2e