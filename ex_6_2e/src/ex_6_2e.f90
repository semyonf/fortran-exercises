! 6.2e в учебнике

program ex_6_2e
    implicit none

    integer, parameter      :: R_ = 4
    integer                 :: Out = 0, In = 0
    character(*), parameter :: output_file = "output.txt", &
                               input_file = "../data/input.txt", &
                               E_ = "UTF-8"

    open (file=input_file, encoding=E_, newunit=In)
    close (In)

    a ** x = e ** (x * log(a)) = 

    open (file=output_file, encoding=E_, newunit=Out)
    close (Out)
end program ex_6_2e