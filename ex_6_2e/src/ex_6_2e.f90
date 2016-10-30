! 6.2e в учебнике

program ex_6_2e
    implicit none

    integer, parameter      :: R_ = 4
    real(R_), parameter     :: RELERR = 0.005
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt", &
                               E_ = "UTF-8"
    real(R_)                :: x = 0, sum = 0, current = 1, a = 0
    integer                 :: In = 0, Out = 0, i = 1

    open (file=input_file, encoding=E_, newunit=In)
        read(In,'(f5.2)') a
        read(In,'(f5.2)') x
    close (In)

    do
        sum = sum + current
        write(*,*) 'current = ', current, 'sum = ', sum
        current = current * x * log(a) / i
        if ((current / sum) < RELERR) then
            exit
        endif
        i = i + 1
    end do

    open (file=output_file, encoding=E_, newunit=Out)
        write(Out, *) sum
    close (Out)

end program ex_6_2e