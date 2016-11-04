! 6.2e в учебнике

program ex_6_2e
    implicit none

    integer, parameter      :: R_ = 4
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt"
    real(R_)                :: x = 0, a = 0, sum = 0, current = 0, new = 0
    integer                 :: In = 0, Out = 0, i = 0

    open (file=input_file, newunit=In)
        read(In,'(f5.2)') a
        read(In,'(f5.2)') x
    close (In)

    current = 1

    do
        i = i + 1
        sum = sum + current
        new = current * x * log(a) / i
        if (new == current) &
            exit
        current = new
    end do

    open (file=output_file, newunit=Out)
        write(Out, *) sum, ' ~ ' ,a**x
    close (Out)

end program ex_6_2e